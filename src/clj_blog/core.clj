(ns clj-blog.core
  (:use markdown.core
        hiccup.core
        hiccup.page
        compojure.core
        clj-blog.config)
  (:require [clojure.string :as str]
            [compojure.handler :as handler]
            [clojure-watch.core :refer [start-watch]]
            [ring.util.response :as res]
            [clojure.java.io :as io]))

(def ^:dynamic *body*)
(def TAGS (atom {}))
(def ARCHIVES (atom {}))

(let [read-file #(read-string (slurp %))
      memoize-read-file (memoize read-file)]
  (defn read-template [f]
    ((if (:dev? config) read-file memoize-read-file)
     (str (:in-dir config) "/templates/" f))))

(defn load-template [f]
  (eval (read-template f)))

(defn html-doc [body]
  (binding [*ns* (the-ns 'clj-blog.core)
            *body* body]
    (html
     (doctype :html5)
     (load-template (or (:template *body*)
                    (:template config))))))

(defmacro defpage [page args & body]
  `(defn ~page ~args
     (html-doc (do ~@body))))

(defpage blog [body & [template]]
  (let [body (if (map? body) body {:content body})]
    (assoc body :template template)))

(defn compile-markdown [f]
  (let [s (slurp f)
        m-start (+ 4 (.indexOf s "---\n"))
        m-end (.indexOf s "---\n" m-start)
        m-str (.substring s m-start m-end)
        c-str (.substring s (+ 4 m-end))]
    {:metadata (as-> m-str $
                     (str/split $ #"\n")
                     (map #(str/split % #"\s*:\s*" 2) $)
                     (map #(do [(keyword (first %)) (second %)]) $)
                     (into {} $))
     :content (md-to-html-string c-str)}))

(defn build-archive [m]
  (let [month-str (.substring (:date-str m) 0 7)]
    (swap! ARCHIVES update-in [month-str] conj m)))

(defn build-tags [tags m]
  (map #(swap! TAGS update-in [%] conj m) tags))

(defn mkdir [f & {make-parents? :make-parents?}]
  (if (instance? String f)
    (mkdir (java.io.File. f) :make-parents? make-parents?)
    (when-not (or (not f) (.exists f))
      (when make-parents?
        (mkdir (.getParentFile f) :make-parents? make-parents?))
      (.mkdir f))))

(defn str-dump [path c]
  (let [f (java.io.File. path)]
    (mkdir (.getParentFile f) :make-parents? true)
    (spit path c)))

(defn dump-blog [post]
  (let [date-str (re-find #"\d{4}-\d\d-\d\d" post)
        title (-> (re-find #"\d{4}-\d\d-\d\d-(.*)" post)
                             second
                             (str/replace #"-" " ")
                             (str/replace #"\.\w+$" ""))
        post-uri (str (str/replace date-str #"-" "/") "/" (str/replace title #" " "-"))
        out-path (str (:out-dir config) "/" post-uri "/" "index.html")
        body (compile-markdown (str (:in-dir config) "/posts/" post))
        body (assoc body :metadata (merge {:date-str date-str
                                           :title title
                                           :type :post
                                           :post-uri post-uri}
                                          (:metadata body)))
        tags (when-let [tags (-> body :metadata :tags)]
               (str/split tags #"\s*,\s*"))]
    (build-archive (:metadata body))
    (build-tags tags (:metadata body))
    (str-dump out-path (blog body (:template (:metadata body))))))


(defn build-blog []
  (let [fs (->> (file-seq (io/file (str (:in-dir config) "/posts")))
                (filter #(not (.isHidden %)))
                (map #(.getPath %))
                (filter #(.endsWith % ".md"))
                (map #(str/replace % (str (:in-dir config) "/posts/") "")))]
    (doall (map dump-blog fs))))

(defn -main []
  (build-blog)
  (future
    (start-watch [{:path (:in-dir config)
                   :event-types [:create :modify :delete]
                   :bootstrap (fn [path] (println "Starting to watch" path))
                   :callback (fn [event filename]
                               (println event filename)
                               (build-blog))
                   :options {:recursive true}}])))


(defn- add-wildcard
  "Add a wildcard to the end of a route path."
  [^String path]
  (str path (if (.endsWith path "/") "*" "/*")))

(defn static [path & [opts]]
  (-> (GET (add-wildcard path) {{resource-path :*} :route-params}
           (let [root (:root opts "public")]
             (res/file-response (str root "/" resource-path))))))

(defroutes app-routes
  (static "/" {:root "html"}))

(def app
  (-> (handler/site app-routes)))
