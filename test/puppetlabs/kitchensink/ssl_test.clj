(ns puppetlabs.kitchensink.ssl-test
  (:import java.util.Arrays
           [java.security PrivateKey])
  (:use clojure.test
        puppetlabs.kitchensink.ssl
        [clojure.java.io :only [resource reader]]))

(deftest privkeys
  (testing "assoc-private-key-file!"
    (let [private-key-file (resource "puppetlabs/kitchensink/examples/ssl/private_keys/localhost.pem")
          cert-file        (resource "puppetlabs/kitchensink/examples/ssl/certs/localhost.pem")
          keystore         (keystore)
          _                (assoc-private-key-file! keystore "mykey" private-key-file "bunkpassword" cert-file)
          keystore-key     (.getKey keystore "mykey" (char-array "bunkpassword"))
          private-key      (pem->private-key private-key-file)]

      (testing "key read from keystore should match key read from pem"
        (is (Arrays/equals (.getEncoded private-key) (.getEncoded keystore-key))))

      (testing "pem created from keystore should match original pem file"
        (let [pem-writer-stream   (java.io.ByteArrayOutputStream.)
              _                   (key->pem! keystore-key pem-writer-stream)]
          (is (Arrays/equals (-> (reader private-key-file)
                                 (slurp)
                                 (.getBytes))
                             (.toByteArray pem-writer-stream))))))))

(deftest multiple-certs
  (testing "loading a PEM file with multiple certs"
    (let [pem (resource "puppetlabs/kitchensink/examples/ssl/certs/multiple.pem")]
      (testing "should return multiple certs"
        (is (= 2 (count (pem->certs pem)))))

      (testing "should load all certs from the file into a keystore"
        (let [ks (keystore)]
          (assoc-certs-from-file! ks "foobar" pem)
          (is (= 2 (.size ks)))
          (is (.containsAlias ks "foobar-0"))
          (is (.containsAlias ks "foobar-1")))))))

(deftest rsakeyonly
  (testing "reading PEM files with only the RSA-key should work"
    (let [privkey (resource "puppetlabs/kitchensink/examples/ssl/private_keys/keyonly.pem")]
      (is (instance? PrivateKey (pem->private-key privkey))))))
