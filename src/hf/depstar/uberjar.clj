(ns hf.depstar.uberjar
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string])
  (:import
   (java.io File InputStream PushbackReader)
   (java.nio.file CopyOption LinkOption OpenOption
                  StandardCopyOption StandardOpenOption
                  FileSystem FileSystems Files
                  FileVisitOption FileVisitResult FileVisitor
                  Path)
   (java.nio.file.attribute FileAttribute FileTime)
   (java.util.jar JarInputStream JarEntry))
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:dynamic *suppress-clash* true)

(def LINK_OPTION
  (make-array LinkOption 0))

(def STRING_OPTION
  (make-array String 0))

(def OPEN_OPTION
  (make-array OpenOption 0))

(def FILE_ATTRIBUTE_OPTION
  (make-array FileAttribute 0))

(defn env-prop
  "Given a setting name, get its Boolean value from the environment,
  validate it, and return the value (or nil if no setting is present)."
  [setting]
  (let [env-setting  (str "DEPSTAR_" (string/upper-case setting))
        prop-setting (str "depstar." (string/lower-case setting))]
    (when-let [level (or
                      (System/getenv env-setting)
                      (System/getProperty prop-setting))]
      (case level
        "true"  true
        "false" false ;; because (if (Boolean. "false") :is-truthy :argh!)
        (throw
         (ex-info
          (str "depstar " setting " should be true or false")
          {:level    level
           :env      (System/getenv env-setting)
           :property (System/getProperty prop-setting)}))))))

(defonce ^FileSystem FS
  (FileSystems/getDefault))

;; could add (StandardOpenOption/valueOf "SYNC") here as well but that
;; could slow things down (and hasn't yet proved to be necessary)
(def open-opts
  (->>
   [(StandardOpenOption/valueOf "CREATE")]
   (into-array OpenOption)))

(def copy-opts
  (->>
   [(StandardCopyOption/valueOf "REPLACE_EXISTING")]
   (into-array CopyOption)))

(def visit-opts
  (doto (java.util.HashSet.)
    (.add (FileVisitOption/valueOf "FOLLOW_LINKS"))))

(defonce errors
  (atom 0))

(defonce multi-release?
  (atom false))

(defn get-path
  ^Path [s]
  (.getPath FS s STRING_OPTION))

(defn clash-strategy
  [filename]
  (cond
    (re-find #"data_readers.clj[sc]?$" filename) :merge-edn
    (re-find #"^META-INF/services/" filename)    :concat-lines
    :else                                        :noop))

(defmulti clash
  (fn [filename in target]
    (let [stategy (clash-strategy filename)]
      (when-not *suppress-clash*
        (pprint {:warning  "clashing jar item"
                 :path     filename
                 :strategy stategy}))
      stategy)))

(defmethod clash
  :merge-edn
  [_ in target]
  (let [;; read but do not close input stream
        f1 (edn/read (PushbackReader. (io/reader in)))
        ;; read and then close target since we will rewrite it
        f2 (with-open [r (PushbackReader. (Files/newBufferedReader target))]
             (edn/read r))]
    (with-open [w (Files/newBufferedWriter target open-opts)]
      (binding [*out* w]
        (prn (merge f1 f2))))))

(defmethod clash
  :concat-lines
  [_ in target]
  (let [f1 (conj (vec (line-seq (io/reader in))) "\n")
        f2 (Files/readAllLines target)
        fs (into f1 f2)]
    (with-open [w (Files/newBufferedWriter target open-opts)]
      (binding [*out* w]
        (run! println fs)))))

(defmethod clash
  :default
  [_ in target]
  ;; do nothing, first file wins
  nil)

(def ^:private exclude-patterns
  "Filename patterns to exclude. These are checked with re-matches and
  should therefore be complete filename matches including any path."
  [#"project.clj"
   #"LICENSE"
   #"COPYRIGHT"
   #"\.keep"
   #".*\.pom$" #"module-info\.class$"
   #"(?i)META-INF/.*\.(?:MF|SF|RSA|DSA)"
   #"(?i)META-INF/(?:INDEX\.LIST|DEPENDENCIES|NOTICE|LICENSE)(?:\.txt)?"])

(defn excluded?
  [filename]
  (let [match? (fn [p] (re-matches p filename))]
    (some match? exclude-patterns)))

(defn copy!
  ;; filename drives strategy
  [filename ^InputStream in ^Path target last-mod]
  (when-not (excluded? filename)
    (if (Files/exists target LINK_OPTION)
      (clash filename in target)
      (do
        (Files/copy in target ^"[Ljava.nio.file.CopyOption;" copy-opts)
        (when last-mod
          (Files/setLastModifiedTime target last-mod))))))

(defn consume-jar
  [^Path path f]
  (with-open
    [is (->>
         (Files/newInputStream path OPEN_OPTION)
         (java.io.BufferedInputStream.)
         (JarInputStream.))]
    (loop []
      (when-let [entry (.getNextJarEntry is)]
        (f is entry)
        (recur)))))

(defn classify
  [entry]
  (let [p (get-path entry)]
    (if (Files/exists p LINK_OPTION)
      (cond
        (Files/isDirectory p LINK_OPTION)
        :directory

        (and
         (Files/isRegularFile p LINK_OPTION)
         (re-find #"\.jar$" (str p)))
        :jar

        :else
        :unknown)
      :not-found)))

(defmulti copy-source*
  (fn [src dest options]
    (classify src)))

(defmethod copy-source*
  :jar
  [src dest options]
  (when-not (= :thin (:jar options))
    (consume-jar
     (get-path src)
     (fn [inputstream ^JarEntry entry]
       (let [^String name (.getName entry)
             last-mod     (.getLastModifiedTime entry)
             target       (.resolve ^Path dest name)]
         (if (.isDirectory entry)
           (Files/createDirectories target FILE_ATTRIBUTE_OPTION)
           (do
             (Files/createDirectories (.getParent target) FILE_ATTRIBUTE_OPTION)
             (try
               (when (.startsWith name "META-INF/versions/")
                 (reset! multi-release? true))
               (copy! name inputstream target last-mod)
               (catch Throwable t
                 (pprint {:error     "unable to copy file"
                          :name      name
                          :exception (class t)
                          :message   (.getMessage t)})
                 (swap! errors inc))))))))))

(defn reify-file-visitor
  [^Path src ^Path dest]
  (reify FileVisitor
    (preVisitDirectory [_ p attrs]
      (let [src  (str (.relativize src p))
            dest (.resolve dest src)]
        (Files/createDirectories dest FILE_ATTRIBUTE_OPTION))
      FileVisitResult/CONTINUE)

    (visitFile [_ p attrs]
      (let [rel-path (.relativize src p)
            last-mod (Files/getLastModifiedTime p LINK_OPTION)]
        (with-open [is (Files/newInputStream p OPEN_OPTION)]
          (let [src  (str rel-path)
                dest (.resolve dest src)]
            (copy! src is dest last-mod))))
      FileVisitResult/CONTINUE)

    (postVisitDirectory [_ p ioexc]
      (if ioexc
        (throw ioexc)
        FileVisitResult/CONTINUE))

    (visitFileFailed [_ p ioexc]
      (throw (ex-info "Visit File Failed" {:p p} ioexc)))))

(defn copy-directory
  [^Path src ^Path dest]
  (let [visitor (reify-file-visitor src dest)]
    (Files/walkFileTree
     src
     visit-opts
     Integer/MAX_VALUE
     visitor)
    :ok))

(defmethod copy-source*
  :directory
  [src dest options]
  (copy-directory (get-path src) dest))

(defmethod copy-source*
  :not-found
  [src _dest _options]
  (pprint {:warning "could not find classpath entry"
           :path    src}))

(defmethod copy-source*
  :unknown
  [src _dest _options]
  (when (excluded? src)
    (pprint {:warning "ignoring unknown file type"
             :path    src})))

(defn copy-source
  [src dest options]
  (copy-source* src dest options))

(defn depstar-itself?
  [p]
  (re-find #"depstar" p))

(defn- first-by-tag
  [pom-text tag]
  (->
   (str "<" (name tag) ">([^<]+)</" (name tag) ">")
   (re-pattern)
   (re-seq pom-text)
   (first)
   (second)))

(defn- copy-pom
  "Using the pom.xml file in the current directory, build a manifest
  and pom.properties, and add both those and the pom.xml file to the JAR."
  [^Path dest {:keys [jar main-class pom-file]}]
  (let [pom-text    (slurp pom-file)
        jdk         (string/replace (System/getProperty "java.version")
                                 #"_.*" "")
        group-id    (first-by-tag pom-text :groupId)
        artifact-id (first-by-tag pom-text :artifactId)
        version     (first-by-tag pom-text :version)
        build-now   (java.util.Date.)
        last-mod    (FileTime/fromMillis (.getTime build-now))
        manifest    (str "Manifest-Version: 1.0\n"
                         "Built-By: depstar\n"
                         "Build-Jdk: " jdk "\n"
                         (when @multi-release?
                           "Multi-Release: true\n")
                         (when-not (= :thin jar)
                           (str "Main-Class: "
                                (if main-class
                                  (string/replace main-class "-" "_")
                                  "clojure.main")
                                "\n")))
        properties  (str "#Generated by depstar\n"
                         "#" build-now "\n"
                         "version: " version "\n"
                         "groupId: " group-id "\n"
                         "artifactId: " artifact-id "\n")
        maven-dir   (str "META-INF/maven/" group-id "/" artifact-id "/")]

    (when-not (and group-id artifact-id version)
      (throw (ex-info "Unable to read pom.xml file!"
                      {:group-id    group-id
                       :artifact-id artifact-id
                       :version     version})))

    (with-open [is (io/input-stream (.getBytes manifest))]
      (let [target (.resolve dest "META-INF/MANIFEST.MF")]
        (Files/createDirectories
         (.getParent target)
         FILE_ATTRIBUTE_OPTION)
        (copy! "MANIFEST.MF" is target last-mod)))

    (with-open [is (io/input-stream (.getBytes properties))]
      (let [target (.resolve dest (str maven-dir "pom.properties"))]
        (Files/createDirectories
         (.getParent target)
         FILE_ATTRIBUTE_OPTION)
        (copy! "pom.properties" is target last-mod)))

    (with-open [is (io/input-stream pom-file)]
      (let [dest (.resolve dest (str maven-dir "pom.xml"))]
        ;; we don't need the createDirectories call here
        (copy! "pom.xml" is dest last-mod)))))

(defn run
  [{:keys [aot dest jar main-class no-pom ^File pom-file suppress classpath]
    :or   {classpath (System/getProperty "java.class.path")
           jar       :uber}
    :as   options}]

  (let [aot?       (and aot main-class (not no-pom) (.exists pom-file))
        tmp-c-dir  (when aot? (Files/createTempDirectory "depstarc" FILE_ATTRIBUTE_OPTION))
        tmp-z-dir  (Files/createTempDirectory "depstarz" FILE_ATTRIBUTE_OPTION)
        classpaths (->>
                    (re-pattern (System/getProperty "path.separator"))
                    (string/split classpath)
                    (into
                     (cond-> [] aot? (conj (str tmp-c-dir)))
                     (remove depstar-itself?)))
        dest-name  (string/replace dest #"^.*[/\\]" "")
        jar-path   (.resolve tmp-z-dir ^String dest-name)
        jar-file   (java.net.URI. (str "jar:" (.toUri jar-path)))
        zip-opts   (doto (java.util.HashMap.)
                     (.put "create" "true")
                     (.put "encoding" "UTF-8"))]

    (when aot?
      (try
        (println "Compiling" main-class "...")
        (binding [*compile-path* (str tmp-c-dir)]
          (compile (symbol main-class)))
        (catch Throwable t
          (throw (ex-info
                  (str "Compilation of " main-class " failed!")
                  (dissoc options :pom-file)
                  t)))))

    (with-open [zfs (FileSystems/newFileSystem jar-file zip-opts)]
      (let [tmp (.getPath zfs "/" STRING_OPTION)]
        (reset! errors 0)
        (reset! multi-release? false)
        (println "Building" (name jar) "jar:" dest)
        (run! #(copy-source % tmp options) classpaths)
        (when (and (not no-pom) (.exists pom-file))
          (copy-pom tmp options))))

    (let [dest-path (get-path dest)]
      (when-let [parent (.getParent dest-path)]
        (.. parent toFile mkdirs))
      (Files/move jar-path dest-path copy-opts))

    (when (pos? @errors)
      (println "\nCompleted with errors!"))))

(defn help []
  (println "library usage:")
  (println "  clojure -A:depstar -m hf.depstar.jar MyProject.jar")
  (println "uberjar usage:")
  (println "  clojure -A:depstar -m hf.depstar.uberjar MyProject.jar")
  (println "options:")
  (println "  -C / --compile -- enable AOT compilation for uberjar")
  (println "  -h / --help    -- show this help (and exit)")
  (println "  -m / --main    -- specify the main namespace (or class)")
  (println "  -n / --no-pom  -- ignore pom.xml")
  (println "  -S / --suppress-clash")
  (println "                 -- suppress warnings about clashing jar items")
  (println "note: the -C and -m options require a pom.xml file")
  (System/exit 1))

(defn uber-main
  [opts args]
  (when (some #(#{"-h" "--help"} %) (cons (:dest opts) args))
    (help))
  (let [aot        (some #(#{"-C" "--compile"} %) args)
        main-class (some #(when (#{"-m" "--main"} (first %)) (second %))
                         (partition 2 1 args))
        no-pom     (some #(#{"-n" "--no-pom"}  %) args)
        pom-file   (io/file "pom.xml")
        suppress   (some #(#{"-S" "--suppress-clash"} %) args)
        aot-main   (if main-class
                     (cond
                       (= :thin (:jar opts))    (println "Ignoring -m / --main because a 'thin' JAR was requested!")
                       no-pom                   (println "Ignoring -m / --main because -n / --no-pom was specified!")
                       (not (.exists pom-file)) (println "Ignoring -m / --main because no 'pom.xml' file is present!")
                       :else                    {:aot aot :main-class main-class})
                     (when aot (println "Ignoring -C / --compile because -m / --main was not specified!")))]
    (->>
     {:pom-file pom-file :no-pom no-pom :suppress suppress}
     (merge opts aot-main)
     (run))))

(defn -main
  [& [destination & args]]
  (when-not destination (help))
  (->
   {:dest destination :jar :uber}
   (uber-main args)))
