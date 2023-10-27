(ns puppetlabs.kitchensink.file
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io BufferedWriter FileOutputStream OutputStreamWriter)
           (java.nio ByteBuffer)
           (java.nio.channels Channels)
           (java.nio.file CopyOption Files LinkOption Path Paths StandardCopyOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.util.zip GZIPInputStream)
           (org.apache.commons.compress.archivers.tar TarArchiveInputStream)))

(defn str->path
  ^Path [path]
  (Paths/get path (into-array String [])))

(defn dir+file->path
  ^Path [directory file]
  (Paths/get directory (into-array String [file])))

(defn get-perms
  "Returns the currently set permissions of the given file path."
  [path]
  (-> (str->path path)
      (Files/getPosixFilePermissions (into-array LinkOption []))
      PosixFilePermissions/toString))

(defn set-perms
  "Set the provided permissions on the given path. The permissions string is in
  the form of the standard 9 character posix format, e.g. \"rwxr-xr-x\"."
  [path permissions]
  (-> (str->path path)
      (Files/setPosixFilePermissions
       (PosixFilePermissions/fromString permissions))
      (.toFile)))

(defn perms->attribute
  [permissions]
  (PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString permissions)))

(def nofollow-links
  (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(def ^:private default-permissions
  (PosixFilePermissions/fromString "rw-r-----"))

(defn atomic-write
  "Write to a file atomically. This takes a function that should operate on a
  Writer as its first argument. If permissions are specified (as a string of the
  form \"rwxrwxrwx\") then the file will be created with those permissions. If
  not specified and the file already exists then existing permissions will be
  preserved. If no file exist a default is set."
  ([path write-function]
   (atomic-write path write-function nil))
  ([path write-function permissions]
   (let [target (str->path path)
         dir (.getParent target)
         file-exists? (Files/exists target nofollow-links)
         owner (when file-exists?
                 (Files/getOwner target nofollow-links))
         group (when file-exists?
                 (Files/getAttribute target "posix:group" nofollow-links))
         permissions (if permissions
                       (PosixFilePermissions/fromString permissions)
                       (if file-exists?
                         (Files/getPosixFilePermissions target nofollow-links)
                         default-permissions))
         temp-attributes (into-array FileAttribute [(perms->attribute "rw-------")])
         temp-file (Files/createTempFile dir (.toString (.getFileName target)) "tmp" temp-attributes)
         stream  (proxy [FileOutputStream] [(.toString temp-file)]
                   (close []
                     (.sync (.getFD ^FileOutputStream this))
                     ;; this looks weird, but makes the proxy-super avoid reflection by masking `this` with a version that has the meta tag
                     (let [^FileOutputStream this this]
                       (proxy-super close))))
         writer (BufferedWriter. (OutputStreamWriter. stream))]

     (write-function writer)

     (.close writer)

     (when owner
       (Files/setOwner temp-file owner))
     (when group
       (Files/setAttribute temp-file "posix:group" group nofollow-links))

     ;; We set these here instead of at file creation to avoid problems with
     ;; read-only files and umask
     (Files/setPosixFilePermissions temp-file permissions)
     (Files/move temp-file target (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE])))))

(defn atomic-write-string
  "Write a string to a file atomically. See atomic-write for more details."
  ([path string]
   (atomic-write-string path string nil))
  ([path string permissions]
   (atomic-write path #(.write ^BufferedWriter % ^String string) permissions)))

(defn unzip-file
  "Given a path to an input file, and a path to an output location, attempt to unzip the file.
  Will not overwrite the same path with the output"
  [input-file output-file]
  (when (not= input-file output-file)
    (io/make-parents output-file)
    (with-open [file-input-stream (io/input-stream input-file)
                gzip-input-stream (GZIPInputStream. file-input-stream)
                file-output-stream (io/output-stream output-file)]
      (io/copy gzip-input-stream file-output-stream))))

(def empty-file-attributes
  (into-array FileAttribute []))

(defn write-tar-stream-to-file
  [^TarArchiveInputStream input-stream ^Path output-path ^ByteBuffer buffer]
  (with-open [out-channel (.getChannel (FileOutputStream. (.toFile output-path)))]
    ;; move the byte buffer out for efficiency
    (let [;; specifically don't close this as it will close the whole tarfile stream
          in-channel (Channels/newChannel input-stream)]
      (loop [in-buffer-size (.read in-channel buffer)]
        (when (or (pos? in-buffer-size) (pos? (.position buffer)))
          (.flip buffer)
          (.write out-channel buffer)
          (.compact buffer)
          (recur (.read in-channel buffer)))))))

(defn untar-file
  "Given a path to a tar file, and a path to the parent directory to untar the repo in, recreate the contents of the tar file
  Note: does not recreate original permissions, or support symlinks."
  [path-to-tar-file output-directory]
  (let [trimmed-output (if (str/ends-with? output-directory "/") (subs output-directory 0 (dec (count output-directory))) output-directory)
        ;; allocate the copy buffer once and reuse for efficiency.
        buffer (ByteBuffer/allocateDirect (* 64 1024))]
    (with-open [tar-input-stream (TarArchiveInputStream. (io/input-stream path-to-tar-file))]
      (loop [entry (.getNextTarEntry tar-input-stream)]
        (when (some? entry)
          (if (.isDirectory entry)
            (Files/createDirectories (dir+file->path trimmed-output (.getName entry)) empty-file-attributes)
            (let [output-file (dir+file->path trimmed-output (.getName entry))]
              (io/make-parents output-file)
              (write-tar-stream-to-file tar-input-stream output-file buffer)))
          (recur (.getNextTarEntry tar-input-stream)))))))