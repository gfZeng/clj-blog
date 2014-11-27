(defproject clj-blog "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [compojure "1.1.8"]
                 [ring/ring-core "1.3.0"]
                 [ring/ring-jetty-adapter "1.3.0"]
                 [markdown-clj "0.9.58"]
                 [hickory "0.5.4"]
                 [clojure-watch "0.1.9"]
                 [hiccup "1.0.5"]]
  :plugins [[lein-ring "0.8.11"]]
  :main clj-blog.core
  :ring {:handler clj-blog.core/app
         :port 2718
         :init clj-blog.core/-main
         ;:auto-refresh? true
         :auto-reload? true})
