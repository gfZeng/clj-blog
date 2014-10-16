;;(doctype :xhtml-transitional)
(let [[metadata content] ((juxt :metadata :content) *body*)]
  [:html
   [:head]
   (hiccup.page/include-js "/scripts/shCore.js")
   (hiccup.page/include-css "/css/shCore.css" "/css/shThemeDefault.css")
   [:body
    [:p [:i (:date-str metadata)]]
    content
    [:script {:type "text/javascript"}
     "SyntaxHighlighter.all()"]]])
