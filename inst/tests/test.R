template <- "<h1>{{header}}</h1>
{{#bug}}{{/bug}}

{{#items}}
{{#first}}
<li><strong>{{name}}</strong></li>
{{/first}}
{{#link}}
<li><a href=\"{{url}}\">{{name}}</a></li>
{{/link}}
{{/items}}

{{=<% %>=}}
<%#empty%>
<p>The list is empty.</p>
<%/empty%>

<%={{  }}=%>
{{#bug}}
{{/bug}}

{{#foo}}{{#foo}}{{foo}}{{/foo}}{{/foo}}"

tree <- parse_template(template)
nested.tree <- nest_parts(tree)

root.template <- rmustachePart(type = "root", value = nested.tree)

root.template$render(data = list("header" = "foo", "empty" = FALSE, "foo" = "bar"))

render_mustache(template, data = list("header" = "foo", "empty" = FALSE, "foo" = "bar"))
