;;(doctype :xhtml-transitional)
(let [[metadata content] ((juxt :metadata :content) *body*)]
  [:html
   [:head]
   (hiccup.page/include-js "/highlight.pack.js")
   (hiccup.page/include-css "/styles/default.css")
   (hiccup.page/include-css "/base.css")
   [:body
    [:div#container
     [:div#nav
      [:div#id
       [:a {:href "#",
            :style "cursor: pointer; text-decoration: none;",
            :onclick "javascript:window.location = window.location.origin"}
        [:pre
         {}
         [:code
          {:class "clojure"}
          "(ns"
          [:span {:class "ns"} " isaac.zeng"]
          ")"]]]]
      [:div#classes
       {:style "margin-top: 16px;"}
       [:ul
        {:style "list-style: none; padding-left: 10px;"}
        [:a {:href ""} [:li {} "Home"]]
        [:a {:href ""} [:li {} "Tags"]]
        [:a {:href ""} [:li {} "Archives"]]
        [:a {:href ""} [:li {} "About"]]]]]
     [:div#content
      [:h3 {} (:title metadata)]
      [:div {:style "margin: 30px 0 100px 0;"}
       content]
      [:div {:id "disqus"}
       (if (and (not (:dev? config)) (= (:type metadata) :post))
         (str
          "<div id=\"disqus_thread\"></div>
    <script type=\"text/javascript\">
        /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
        var disqus_shortname = 'isaac-zeng'; // required: replace example with your forum shortname

        /* * * DON'T EDIT BELOW THIS LINE * * */
        (function() {
            var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
            dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
            (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
        })();
    </script>
    <noscript>Please enable JavaScript to view the <a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>
    "))]]
      [:script {:type "text/javascript"}
       "hljs.initHighlightingOnLoad();"]]]])
