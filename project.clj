(defproject provisdom/todo "0.1.0"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.238"]
                 [org.clojure/spec.alpha "0.1.143"]
                 [org.clojure/core.async "0.3.465"]
                 [org.clojure/test.check "0.9.0"]
                 [io.replikativ/hasch "0.3.4"]
                 [rum "0.11.0"]
                 [thheller/shadow-cljs "2.2.25"]
                 [provisdom/maali "0.1.0"]]
  :repositories [["clojars" {:url "https://repo.clojars.org/"}] ["maven-central" {:url "https://repo1.maven.org/maven2"}]]
  :source-paths ["src" "test"]
  :cljsbuild {:builds [{:id           "dev"
                        :source-paths ["src" "test" "dev"]
                        :dependencies [thheller/shadow-cljs "2.2.25"]
                        :compiler     {:main "provisdom.todo.app"
                                       :asset-path "cljs/out"
                                       :output-to  "resources/public/cljs/main.js"
                                       :output-dir "resources/public/cljs/out"
                                       :verbose false
                                       :parallel-build true}}]})
