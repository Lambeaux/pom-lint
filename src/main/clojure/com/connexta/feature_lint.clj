(ns com.connexta.feature-lint
  (:require [clojure.pprint :refer [pprint]]
            [clojure.xml :as xml]
            [clojure.set :as sets]
            [clojure.data :as data]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def ddf-feature-files
  ["/cx/repos/codice/ddf/broker/broker-app/target/classes/features.xml"
   "/cx/repos/codice/ddf/catalog/ui/search-ui-app/target/classes/features.xml"
   "/cx/repos/codice/ddf/catalog/solr/solr-app/target/classes/features.xml"
   "/cx/repos/codice/ddf/catalog/catalog-app/target/classes/features.xml"
   "/cx/repos/codice/ddf/catalog/resourcemanagement/resourcemanagement-app/target/classes/features.xml"
   "/cx/repos/codice/ddf/catalog/spatial/spatial-app/target/classes/features.xml"
   "/cx/repos/codice/ddf/catalog/spatial/geowebcache/geowebcache-app/target/classes/features.xml"
   "/cx/repos/codice/ddf/catalog/spatial/registry/registry-app/target/classes/features.xml"
   "/cx/repos/codice/ddf/platform/admin/admin-app/target/classes/features.xml"
   "/cx/repos/codice/ddf/features/karaf/target/classes/features.xml"
   "/cx/repos/codice/ddf/features/branding/target/classes/features.xml"
   "/cx/repos/codice/ddf/features/test-utilities/target/classes/features.xml"
   "/cx/repos/codice/ddf/features/security/target/classes/features.xml"
   "/cx/repos/codice/ddf/features/camel/target/classes/features.xml"
   "/cx/repos/codice/ddf/features/admin/target/classes/features.xml"
   "/cx/repos/codice/ddf/features/install-profiles/target/classes/features.xml"
   "/cx/repos/codice/ddf/features/solr/target/classes/features.xml"
   "/cx/repos/codice/ddf/features/utilities/target/classes/features.xml"
   "/cx/repos/codice/ddf/features/cxf/target/classes/features.xml"
   "/cx/repos/codice/ddf/features/apps/target/classes/features.xml"
   "/cx/repos/codice/ddf/features/migration/target/classes/features.xml"
   "/cx/repos/codice/ddf/features/notifications/target/classes/features.xml"
   "/cx/repos/codice/ddf/features/kernel/target/classes/features.xml"
   "/cx/repos/codice/ddf/distribution/test/itests/test-itests-dependencies-app/target/classes/features.xml"
   "/cx/repos/codice/ddf/distribution/sdk/sample-plugins/target/classes/features.xml"
   "/cx/repos/codice/ddf/distribution/sdk/sdk-app/target/classes/features.xml"
   "/cx/repos/codice/ddf/distribution/sdk/sample-metacard-filter/target/classes/features.xml"])

(def feature-xml-header-attrs
  {:xsi:schemaLocation "http://karaf.apache.org/xmlns/features/v1.3.0 http://karaf.apache.org/xmlns/features/v1.3.0"
   :xmlns              "http://karaf.apache.org/xmlns/features/v1.3.0",
   :name               "flattened-2.19.4",
   :xmlns:xsi          "http://www.w3.org/2001/XMLSchema-instance"})

;; ----------------------------------------------------------------------
;; # XML
;;
;; Functions for parsing, navigating, and validating generic XML structures. Note that some
;; feature file assumptions might still be present (i.e. string cleaning).

(defn- xml-parse
  "Parses the file at the given string path."
  [path]
  (let [file (io/file path)]
    (if-not (.exists file)
      (throw (IllegalArgumentException. (str "Path does not exist: " path)))
      (xml/parse file))))

(defn- xml-terminal-content
  "Mapper for a feature XML leaf node, to get the string content of the node."
  [node]
  (first (:content node)))

(defn- xml-string-clean
  "Validates the string content of feature XML leaf nodes and removes horizontal and vertical
  whitespace surrounding the true string, if present."
  [text]
  (let [regex #"([\h|\v]*)([\w|\p{Punct}]+)([\h|\v]*)"
        matcher! (.matcher regex text)
        matches? (.matches matcher!)]
    (if-not matches?
      (throw (IllegalArgumentException.
               (str "invalid bundle name, feature name, or xml string: " text)))
      (.group matcher! 2))))

;; ----------------------------------------------------------------------
;; # Features
;;
;; Work with specific feature data, from either XML directly or files on disk.

(defn- get-all-feature-files
  "Given a string path to a root Maven project, returns a coll of absolute, fully
  qualified path strings that point to Karaf feature files:

  ( /cx/repos/codice/ddf/broker/broker-app/target/classes/features.xml
    /cx/repos/codice/ddf/catalog/ui/search-ui-app/target/classes/features.xml
    /cx/repos/codice/ddf/catalog/solr/solr-app/target/classes/features.xml )
  "
  [root-project-path]
  (let [is-valid-feature-source?
        ; Note that shifting to use the src version might include 'feature.xml', singular.
        #(and (= (.getName %) "features.xml"), (.contains (.getParent %) "/target/classes"))]
    (->> root-project-path
         io/file
         file-seq
         (filter is-valid-feature-source?)
         (map #(.getPath %)))))

(defn- mvn-feature-repo->filepath
  "Converts a maven coordinate pointing to a features repo into a path to the actual features file."
  [coord-string]
  (let [home (System/getProperty "user.home")
        parts (vec (.split (.substring coord-string 4) "/"))
        group (parts 0)
        artifact-id (parts 1)
        version (parts 2)]
    (cond
      (not (.startsWith coord-string "mvn:"))
      (throw (IllegalArgumentException. (str "invalid maven coordinate string: " coord-string)))

      (nil? home)
      (throw (IllegalArgumentException. "user.home not set"))

      :else
      (str home "/.m2/repository/"
           (str/replace group "." "/")
           "/" artifact-id "/" version "/" artifact-id "-" version "-features.xml"))))

(defn- get-feature-repos
  "Returns a coll of strings which are all the feature repos defined in the xml:

  ( mvn:ddf.features/admin/2.19.4/xml/features
    mvn:ddf.features/branding/2.19.4/xml/features
    mvn:ddf.features/camel/2.19.4/xml/features )
   "
  [xml]
  (->> xml
       :content
       (filter #(= (:tag %) :repository))
       (map xml-terminal-content)
       (map xml-string-clean)))

(defn- get-feature-defs
  "Returns features mapped to a coll of their dependencies, which can be other features or
  bundles:

  { catalog-core-directorymonitor (apache-commons,
                                   catalog-transformer-bootflag
                                   mvn:com.hazelcast/hazelcast/3.2.1
                                   mvn:ddf.catalog.core/catalog-core-camelcontext/2.19.4) }
  "
  [xml]
  (let [bundle-or-feature #(or (= (:tag %) :bundle), (= (:tag %) :feature))]
    (->> xml
         :content
         (filter #(= (:tag %) :feature))
         (map #(vector (get-in % [:attrs :name])
                       (->> %
                            :content
                            (filter bundle-or-feature)
                            (map xml-terminal-content)
                            (map xml-string-clean))))
         (into {}))))

;; ----------------------------------------------------------------------
;; # Feature Flattening
;;
;; Functions for traversing feature repository links and converting a single feature into a
;; flat list of its defining bundles.

(defn- xml->layered-feature
  [feature-name xml]
  (let [feature-map (get-feature-defs xml)
        children (feature-map feature-name)
        children-validated (if (not (nil? children))
                             children
                             (throw (IllegalArgumentException.
                                      (str "feature not found: " feature-name))))]
    {:repo-locations    (get-feature-repos xml)
     :repos-processed   #{}
     :feature-tree      feature-map                         ; Todo! Did I forget to init this properly?
     :root-feature-name feature-name
     :children          children-validated}))

(defn- pull-repos-up
  "Update the current :repo-features map with all features contained within the repositories
  in :repo-locations then update :repo-locations with the next set of repository links. "
  [layer-in]
  (let [{repo-locations    :repo-locations
         repos-processed   :repos-processed
         feature-tree      :feature-tree
         root-feature-name :root-feature-name
         children          :children} layer-in
        repo-xml (->> repo-locations
                      (map mvn-feature-repo->filepath)
                      (map xml-parse))]
    {:repo-locations    (->> repo-xml
                             (map get-feature-repos)
                             flatten
                             distinct
                             (filter #(not (contains? repos-processed %))))
     :repos-processed   (sets/union repos-processed (into #{} repo-locations))
     :feature-tree      (->> repo-xml
                             (map get-feature-defs)
                             (apply merge)
                             (merge feature-tree))
     :root-feature-name root-feature-name
     :children          children}))

(defn- pull-features-up
  "Do a single pass of feature substitution from the :feature-tree map onto the :children coll
  for the current :root-feature-name being processed. Verified impl notes:
  - the (flatten) function will preserve element ordering
  - intermediate features will linger"                      ; Todo! Why does that happen again?
  [layer-in]
  (let [{repo-locations    :repo-locations
         repos-processed   :repos-processed
         feature-tree      :feature-tree
         root-feature-name :root-feature-name
         children          :children} layer-in]
    {:repo-locations    repo-locations
     :repos-processed   repos-processed
     :feature-tree      feature-tree
     :root-feature-name root-feature-name
     :children          (->> children
                             (map #(if (contains? feature-tree %) (feature-tree %) %))
                             flatten
                             distinct)}))

(defn- flatten-feature
  "Runs the flattening process, repos first followed by features, until no further changes
  occur."
  [layer-in]
  (let [layer-repos
        (loop [layer layer-in]
          (let [next (pull-repos-up layer)]
            (if (empty? (:repo-locations next))
              next
              (recur next))))
        layer-flattened
        (loop [layer layer-repos]
          (let [next (pull-features-up layer)]
            (if (= (count (:children next))
                   (count (:children layer)))               ; Todo! Should we be using counts? xx
              layer                                         ; Todo! What if we return 'layer' instead of 'next' to ensure zero redundant shifts occurred?
              (recur next))))]
    layer-flattened))

(defn- layered-feature->xml
  "Spits out XML of the given layered feature."
  [layer-in]
  (let [xml-bundle #(->> [%] (vector :content) (conj {:tag :bundle :attrs nil}))
        content (->> layer-in
                     :children
                     ; We assume lingering non-terminal features are extras due to cycles
                     (filter #(or (.startsWith % "mvn:") (.startsWith % "wrap:")))
                     (map xml-bundle)
                     (into []))
        xml {:tag     :features
             :attrs   feature-xml-header-attrs
             :content [{:tag     :feature
                        :attrs   {:name "flat" :description "Bundles only" :version "2.19.4"}
                        :content content}]}]
    (-> xml
        xml/emit
        with-out-str
        (.replace \' \")
        (.replace "&" "&amp;"))))

(defn- xml-write [path xmlstr]
  (spit path xmlstr :create true)
  #_(with-open [out-file (clojure.java.io/writer path :encoding "UTF-8")] ()))

;; ----------------------------------------------------------------------
;; # Sample Usage
;;
;; What composition of the above functions might look like.

(comment
  "Locate all processed feature files for DDF."
  (get-all-feature-files "/cx/repos/codice/ddf"))

(comment
  "Note that the feature:repo-add Karaf command takes a URI, not a path or URL."
  (.toString (.toURI (java.io.File. "/cx/deploy/ddf-2.19.4/features-flat.xml"))))

(comment
  "Flattens the 'profile-standard' feature and writes it to a new features file."
  (->> "/cx/repos/codice/ddf/features/install-profiles/target/classes/features.xml"
       xml-parse
       (xml->layered-feature "profile-standard")
       flatten-feature
       layered-feature->xml
       (xml-write "/cx/deploy/ddf-2.19.4/features-flat.xml")))

(comment
  "Flattens the 'profile-standard' feature and prints the list as Clojure."
  (->> "/cx/repos/codice/ddf/features/install-profiles/target/classes/features.xml"
       xml-parse
       (xml->layered-feature "profile-standard")
       flatten-feature
       ;pull-features-up
       :children
       ;(filter #(.contains % "jetty"))
       count))                                              ; 598

(comment
  (let [bundles-only #(or (.startsWith % "mvn:") (.startsWith % "wrap:"))
        flat (->> "/cx/repos/codice/ddf/features/install-profiles/target/classes/features.xml"
                  xml-parse
                  (xml->layered-feature "profile-standard")
                  flatten-feature)
        shifts-min (:children flat)
        shifts-xtra (:children (pull-features-up flat))]
    (filter bundles-only shifts-xtra)
    #_(data/diff (filter bundles-only shifts-min)
                 (filter bundles-only shifts-xtra)))
  (data/diff '("s" "t" "x" "y" "a" "b" "c" "d") '("s" "t" "y" "a" "c" "b")))

(comment
  "Retrieve specific feature mappings from the generated feature tree."
  (let [tree (->> "/cx/repos/codice/ddf/features/install-profiles/target/classes/features.xml"
                  xml-parse
                  (xml->layered-feature "profile-standard")
                  flatten-feature
                  :feature-tree)]
    (tree "ui")))

(comment
  "Verify that lingering features did have valid mappings in the :feature-tree in case something
  was missed."
  (let [flat (->> "/cx/repos/codice/ddf/features/install-profiles/target/classes/features.xml"
                  xml-parse
                  (xml->layered-feature "profile-standard")
                  flatten-feature)
        {feature-tree :feature-tree
         children     :children} flat]
    (->> children
         (filter #(not (or (.startsWith % "mvn:") (.startsWith % "wrap:"))))
         count                                              ; 47
         #_(map #(contains? feature-tree %))
         #_(reduce #(and %1 %2)))))                         ; true

(defn- expand [f x]
  (if (seq? x) (expand f x) (f x)))
(comment
  "Proof that there are cycles in the features files, a StackOverflowError is expected."
  (let [flat (->> "/cx/repos/codice/ddf/features/install-profiles/target/classes/features.xml"
                  xml-parse
                  (xml->layered-feature "profile-standard")
                  flatten-feature)
        {feature-tree :feature-tree
         children     :children} flat
        sub #(if (contains? feature-tree %) (feature-tree %) %)
        expand-with #(expand sub %)]
    (->> children
         (filter #(or (.contains % "security-")))
         (map expand-with)
         (map expand-with))))

(comment
  "Alternate analysis that there are cycles in the feature files, value frequencies grow without
  bound."
  (let [flat (->> "/cx/repos/codice/ddf/features/install-profiles/target/classes/features.xml"
                  xml-parse
                  (xml->layered-feature "profile-standard")
                  flatten-feature)
        {feature-tree :feature-tree
         children     :children} flat]
    (let [sub #(if (contains? feature-tree %) (feature-tree %) %)
          sub-flat #(->> % (map sub) (flatten))
          freqs (->> children
                     (filter #(or (.contains % "security-")))
                     sub-flat
                     sub-flat
                     sub-flat
                     sub-flat
                     frequencies)
          sort-by-value (fn [k1 k2] (compare [(freqs k2) k2] [(freqs k1) k1]))]
      (into (sorted-map-by sort-by-value) freqs))))

(defn- visit-tree
  ([tree]
   {:feature-tree tree
    :visited      #{}
    :path         []
    :cycles       '()})
  ([out in]
   ()))
(comment
  "Tracing the feature tree for actual cycles."
  (let [tree (->> "/cx/repos/codice/ddf/features/install-profiles/target/classes/features.xml"
                  xml-parse
                  (xml->layered-feature "profile-standard")
                  flatten-feature
                  :feature-tree)]
    ()))

(comment
  "Generic exploration of the layered feature after flattening. Allows exploring individual
  processing steps of the flattening transform. 'do-substitution' performs the initial step,
  and 'make-flat' performs an entire pass of flattening so the results can be compared."
  (let [flat (->> "/cx/repos/codice/ddf/features/install-profiles/target/classes/features.xml"
                  xml-parse
                  (xml->layered-feature "profile-standard")
                  pull-repos-up
                  pull-features-up
                  pull-features-up
                  pull-repos-up
                  pull-features-up
                  #_flatten-feature)
        {repo-locations    :repo-locations
         repos-processed   :repos-processed
         feature-tree      :feature-tree
         root-feature-name :root-feature-name
         children          :children} flat]
    (let [sub #(if (contains? feature-tree %) (feature-tree %) %)
          sub-flat #(->> % (map sub) (flatten))]
      (->> children
           #_(filter #(or (.contains % "security-")))
           #_sub-flat))))
