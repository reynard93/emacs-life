html-mode

;; Hugo
(allpages & "{{ range .Site.RegularPages }}" n> "<a href={{ .Permalink }}>{{ .Title }}</a>" n> "{{ end }}")
(sectionpages & "{{ range where .Site.RegularPages \"Section\" \"" p "\" }}" n> "<a href={{ .Permalink }}>{{ .Title }}</a>" n> "{{ end }}")
(singlepage & "<h1>{{ .Title }}</h1><article>{{ .Content }}</article>")
(pagination & "{{ if .Prev }}<a href={{ .Prev.Permalink}} >{{ .Prev.Title }}</a>{{ end }}" n> "{{ if .Next }}<a href={{ .Next.Permalink}} >{{ .Next.Title }}</a>{{ end }}")
