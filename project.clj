(defproject card-game "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [ring-server "0.5.0"]
                 [reagent "0.8.1"]
                 [reagent-utils "0.3.3"]
                 [ring "1.7.1"]
                 [ring/ring-defaults "0.3.2"]
                 [ring/ring-anti-forgery "1.3.0"]
                 [hiccup "1.0.5"]
                 [yogthos/config "1.1.3"]
                 [org.clojure/clojurescript "1.10.520"
                  :scope "provided"]
                 [metosin/reitit "0.3.7"]
                 [pez/clerk "1.0.0"]
                 [venantius/accountant "0.2.4"
                  :exclusions [org.clojure/tools.reader]]

                 [clojure.java-time "0.3.2"]
                 [com.taoensso/sente "1.14.0-RC2"]
                 [prismatic/plumbing "0.5.5"]]

  :plugins [[lein-environ "1.1.0"]
            [lein-cljsbuild "1.1.7"]
            [lein-asset-minifier "0.4.6"
             :exclusions [org.clojure/clojure]]]

  :ring {:handler card-game.handler/app
         :uberwar-name "card-game.war"}

  :min-lein-version "2.5.0"
  :uberjar-name "card-game.jar"
  :main card-game.server
  :clean-targets ^{:protect false}
  [:target-path
   [:cljsbuild :builds :app :compiler :output-dir]
   [:cljsbuild :builds :app :compiler :output-to]]

  :source-paths ["src/clj" "src/cljc" "src/cljs"]
  :resource-paths ["resources" "target/cljsbuild"]

  :minify-assets
  [[:css {:source "resources/public/css/site.css"
          :target "resources/public/css/site.min.css"}]]

  :cljsbuild
  {:builds {:min
            {:source-paths ["src/cljs" "src/cljc" "env/prod/cljs"]
             :compiler
             {:output-to        "target/cljsbuild/public/js/app.js"
              :output-dir       "target/cljsbuild/public/js"
              :source-map       "target/cljsbuild/public/js/app.js.map"
              :optimizations :advanced
              :infer-externs true
              :pretty-print  false}}
            :app
            {:source-paths ["src/cljs" "src/cljc" "env/dev/cljs"]
             :figwheel {:on-jsload "card-game.core/mount-root"}
             :compiler
             {:main "card-game.dev"
              :asset-path "/js/out"
              :output-to "target/cljsbuild/public/js/app.js"
              :output-dir "target/cljsbuild/public/js/out"
              :source-map true
              :optimizations :none
              :pretty-print  true}}



            }
   }

  :figwheel
  {:http-server-root "public"
   :server-port 3449
   :nrepl-port 7002
   :nrepl-middleware [cider.piggieback/wrap-cljs-repl
                      ]
   :css-dirs ["resources/public/css"]
   :ring-handler card-game.handler/app}



  :profiles {:dev {:repl-options {:init-ns card-game.repl}
                   :dependencies [[cider/piggieback "0.4.1"]
                                  [binaryage/devtools "0.9.10"]
                                  [ring/ring-mock "0.4.0"]
                                  [ring/ring-devel "1.7.1"]
                                  [prone "1.6.3"]
                                  [figwheel-sidecar "0.5.18"]
                                  [nrepl "0.6.0"]
                                  [pjstadig/humane-test-output "0.9.0"]

                                  ]

                   :source-paths ["env/dev/clj"]
                   :plugins [[lein-figwheel "0.5.18"]
                             ]

                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]

                   :env {:dev true}}

             :uberjar {:hooks [minify-assets.plugin/hooks]
                       :source-paths ["env/prod/clj"]
                       :prep-tasks ["compile" ["cljsbuild" "once" "min"]]
                       :env {:production true}
                       :aot :all
                       :omit-source true}})
