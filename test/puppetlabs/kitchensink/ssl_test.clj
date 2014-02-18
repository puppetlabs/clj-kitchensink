(ns puppetlabs.kitchensink.ssl-test
  (:import java.util.Arrays
           [java.security PrivateKey KeyStore]
           (javax.net.ssl SSLContext))
  (:require [clojure.test :refer :all]
            [puppetlabs.kitchensink.ssl :refer :all]
            [puppetlabs.kitchensink.core :as ks]
            [clojure.java.io :refer [resource reader]]))

(deftest privkeys
  (testing "assoc-private-key-file!"
    (let [private-key-file (resource "puppetlabs/kitchensink/examples/ssl/private_keys/localhost.pem")
          cert-file        (resource "puppetlabs/kitchensink/examples/ssl/certs/localhost.pem")
          keystore         (keystore)
          _                (assoc-private-key-file! keystore "mykey" private-key-file "bunkpassword" cert-file)
          keystore-key     (.getKey keystore "mykey" (char-array "bunkpassword"))
          private-key      (first (pem->private-keys private-key-file))]

      (testing "key read from keystore should match key read from pem"
        (is (Arrays/equals (.getEncoded private-key) (.getEncoded keystore-key))))

      (testing "pem created from keystore should match original pem file"
        (let [pem-writer-stream   (java.io.ByteArrayOutputStream.)
              _                   (key->pem! keystore-key pem-writer-stream)]
          (is (Arrays/equals (-> (reader private-key-file)
                                 (slurp)
                                 (.getBytes))
                             (.toByteArray pem-writer-stream))))))))

(deftest multiple-objs
  (testing "loading a PEM file with multiple keys"
    (let [pem (resource "puppetlabs/kitchensink/examples/ssl/private_keys/multiple_pks.pem")]
      (testing "should return multiple keys"
        (is (= 2 (count (pem->private-keys pem)))))))

  (testing "loading compound keys files into a keystore should fail"
    (let [key  (resource "puppetlabs/kitchensink/examples/ssl/private_keys/multiple_pks.pem")
          cert (resource "puppetlabs/kitchensink/examples/ssl/certs/multiple.pem")
          ks   (keystore)]
      (is (thrown? IllegalArgumentException
                   (assoc-private-key-file! ks "foo" key "foo" cert)))))

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
      (is (every? #(instance? PrivateKey %) (pem->private-keys privkey))))))

(deftest pems->keystores-test
  (testing "should be able to convert pems to keystore/truststore"
    (let [result (pems->key-and-trust-stores
                   (resource "puppetlabs/kitchensink/examples/ssl/certs/localhost.pem")
                   (resource "puppetlabs/kitchensink/examples/ssl/private_keys/localhost.pem")
                   (resource "puppetlabs/kitchensink/examples/ssl/certs/ca.pem")                   )]
      (is (map? result))
      (is (= #{:keystore :keystore-pw :truststore} (ks/keyset result)))
      (is (instance? KeyStore (:keystore result)))
      (is (instance? KeyStore (:truststore result)))
      (is (string? (:keystore-pw result))))))

(deftest pems->ssl-context-test
  (testing "should be able to convert pems to SSLContext"
    (let [result (pems->ssl-context
                   (resource "puppetlabs/kitchensink/examples/ssl/certs/localhost.pem")
                   (resource "puppetlabs/kitchensink/examples/ssl/private_keys/localhost.pem")
                   (resource "puppetlabs/kitchensink/examples/ssl/certs/ca.pem"))]
      (is (instance? SSLContext result)))))
