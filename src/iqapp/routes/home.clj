(ns iqapp.routes.home
  (:require [iqapp.layout :as layout]
            [compojure.core :refer [defroutes GET]]
            [clojure.java.io :as io]
            [iqapp.model :as model]))

(defn home-page []
  (layout/render
    "home.html" {:docs (-> "docs/docs.md" io/resource slurp)}))

(defn about-page []
  (layout/render "about.html"))

(defn learn-page []
  (layout/render
    "algo.html" {:algo (model/get-algo sample-data)}))

(defroutes home-routes
  (GET "/" [] (home-page))
  (POST "/learn" [] (learn-page))
  (GET "/about" [] (about-page)))
