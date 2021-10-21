(ns status-im.ui.screens.wallet.swap.views
  (:require [quo.core :as quo]
            [status-im.ui.components.topbar :as topbar]
            [status-im.utils.handlers :refer [<sub]]))

(defn swap []
  (let [{:keys [name]} (<sub [:multiaccount/current-account])]
    [:<>
     [topbar/topbar
      {:title  name
       :modal? true}]
     [quo/text "hello"]]))

