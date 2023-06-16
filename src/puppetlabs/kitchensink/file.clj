(ns puppetlabs.kitchensink.file
  (:import [java.io BufferedWriter FileOutputStream OutputStreamWriter]
           [java.nio.file CopyOption Files LinkOption Paths StandardCopyOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]))

(defn str->path
  [path]
  (Paths/get path (into-array String [])))

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
         stream (proxy [FileOutputStream] [(.toString temp-file)]
                  (close []
                    (.sync (.getFD this))
                    (proxy-super close)))
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
   (atomic-write path #(.write % string) permissions)))
