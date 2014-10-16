(ns clj-blog.config)

(let [default-config {:in-dir "resources"
                      :template "default.clj"
                      :dev? true
                      :out-dir "html"}]
  (def config (merge default-config
                     (let [f (clojure.java.io/file "config.clj")]
                       (when (.exists f)
                         (load-file "config.clj"))))))
