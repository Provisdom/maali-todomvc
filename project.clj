(defproject provisdom/todo "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.9.946"]
                 [org.clojure/spec.alpha "0.1.143"]
                 [org.clojure/core.async "0.3.465"]
                 [org.clojure/test.check "0.9.0"]
                 [io.replikativ/hasch "0.3.4"]
                 [rum "0.11.0"]
                 [thheller/shadow-cljs "2.2.1"]
                 [provisdom/maali "0.0.1-SNAPSHOT"]]
  :repositories [["clojars" {:url "https://repo.clojars.org/"}] ["maven-central" {:url "https://repo1.maven.org/maven2"}]]
  :source-paths ["src" "test"]
  :plugins [[lein-figwheel "0.5.14"]]
  :cljsbuild {:builds [{:id           "dev"
                        :source-paths ["src" "test"]}]})
