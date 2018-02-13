(defproject provisdom/todo "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.9.946"]
                 [org.clojure/spec.alpha "0.1.143"]
                 [org.clojure/core.async "0.3.465"]
                 [org.clojure/test.check "0.9.0"]
                 [io.replikativ/hasch "0.3.4"]
                 [rum "0.11.0"]
                 [alter-cljs "0.2.0"]
                 [provisdom/maali "0.0.1-SNAPSHOT"]]
  :repositories [["clojars" {:url "https://repo.clojars.org/"}] ["maven-central" {:url "https://repo1.maven.org/maven2"}]]
  :source-paths ["src" "test" "resources"]
  :plugins [[lein-figwheel "0.5.14"]]
  :clean-targets ^{:protect false} [:target-path "out" "resources/public/cljs"]
  :cljsbuild {:builds [{:id           "dev"
                        :source-paths ["src" "test"]
                        :figwheel     {:on-jsload "provisdom.todo.app/reload"}
                        :compiler     {:main "provisdom.todo.app"
                                       :asset-path "cljs/out"
                                       :output-to  "resources/public/cljs/main.js"
                                       :output-dir "resources/public/cljs/out"
                                       :verbose false
                                       :parallel-build true}}]}
  :figwheel {:css-dirs ["resources/public/css"]
             :reload-clj-files {:clj false :cljc false}})
