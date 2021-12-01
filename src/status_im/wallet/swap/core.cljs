(ns status-im.wallet.swap.core
  (:require [status-im.utils.fx :as fx]
            [re-frame.db :as re-frame.db]
            [status-im.navigation :as navigation]
            [status-im.ethereum.tokens :as tokens]))

(fx/defn open-asset-selector-modal
  "source? true signinfies we are selecting the source asset. false implies selection of sink asset"
  {:events [::open-asset-selector-modal]}
  [{:keys [db]} source?]
  (fx/merge {:db (assoc db :wallet/modal-selecting-source-token? source?)}
            (navigation/open-modal :swap-asset-selector {})))

(fx/defn set-from-token
  {:events [::set-from-token]}
  [{:keys [db]} from-symbol]
  (fx/merge {:db (assoc db :wallet/swap-from-token from-symbol)}
            (navigation/navigate-back)))

(fx/defn set-to-token
  {:events [::set-to-token]}
  [{:keys [db]} to-symbol]
  (fx/merge {:db (assoc db :wallet/swap-to-token to-symbol)}
            (navigation/navigate-back)))

(fx/defn set-from-token-amount
  [{:keys [db]} from-amount]
  {:db (assoc db :wallet/swap-from-token-amount from-amount)})

(fx/defn set-max-from-token-amount
  [{:keys [db]} _]
  {:db (assoc db :wallet/swap-from-token-amount 0)})

(comment
  (->> re-frame.db/app-db
       deref
      :wallet/all-tokens
      vals
      (map #(str (:name %) "-" (:symbol %)))
      )
  )

