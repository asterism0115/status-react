(ns status-im.ui.screens.wallet.swap.views
  (:require [quo.core :as quo]
            [quo.design-system.colors :as colors]
            [re-frame.core :as re-frame]
            [status-im.ethereum.tokens :as tokens]
            [status-im.ui.components.icons.icons :as icons]
            [status-im.ui.components.keyboard-avoid-presentation
             :as
             kb-presentation]
            [status-im.ui.components.react :as react]
            [status-im.ui.components.slider :as slider]
            [status-im.ui.components.toolbar :as toolbar]
            [status-im.ui.components.topbar :as topbar]
            [status-im.ui.screens.wallet.accounts.views :as accounts]
            [status-im.utils.handlers :refer [<sub]]
            [status-im.i18n.i18n :as i18n]
            [status-im.ui.components.search-input.view :as search-input]))

(defn asset-selector []
  (let [{:keys [address]} (<sub [:multiaccount/current-account])
        {:keys [tokens]}  (<sub [:wallet/visible-assets-with-values address])
        currency          (<sub [:wallet/currency])]
    [:<>
     [topbar/topbar
      {:title  (if true
                 (i18n/label :t/select-token-to-swap)
                 (i18n/label :t/select-token-to-receive))
       :modal? true}]

     [search-input/search-input
      {:search-active? true}]

     [react/scroll-view
      (for [token tokens]
        ^{:key (:name token)}
        [accounts/render-asset token nil nil (:code currency)])]]))

(defn token-display
  "Show token and act as an anchor to open selector"
  [token]
  (let [token-icon-source (-> token :icon :source)]
    [react/touchable-highlight
     {:on-press #(re-frame/dispatch [:open-modal :swap-asset-selector])}
     [react/view {:style {:flex-direction     :row
                          :align-items        :center
                          :border-width       1
                          :border-color       colors/gray-lighter
                          :border-radius      8
                          :margin-left        16
                          :padding-horizontal 8
                          :padding-vertical   2}
                  :accessibility-label
                  :choose-asset-button}
      [quo/text {:style {:margin-right 8}}
       (-> token :symbol name)]
      [react/image {:source (if (fn? token-icon-source)
                              (token-icon-source)
                              token-icon-source)}]]]))

(defn token-input
  "Component to get the amount and type of tokens"
  [{:keys [amount error label token max-from]}]
  (let [window-width (<sub [:dimensions/window-width])]
    [react/view {:style {:justify-content :space-between
                         :flex-direction  :row
                         :align-items     :center}}
     [react/view {:flex 2}
      [react/view {:flex-direction :row
                   :align-items    :center}
       [quo/text label]
       (when max-from [react/touchable-opacity {:style {:background-color   colors/blue-light
                                                        :padding-horizontal 12
                                                        :padding-vertical   2
                                                        :border-radius      24
                                                        :margin-left        16}}
                       [quo/text {:color  :link
                                  :weight :medium} "Max: 1233"]])]
      [react/text-input
       {:style               {:font-size 38
                              :max-width (- (* (/ window-width 4) 3) 106)
                              :color     (if error colors/red colors/black)}
        :keyboard-type       :decimal-pad
        :auto-capitalize     :words
        :accessibility-label :amount-input
        :default-value       amount
        :editable            true
        :auto-focus          true
        :on-change-text      #()
        :placeholder         "0.0"}]]
     [token-display token]]))

(defn separator-with-icon []
  [react/view {:margin-vertical 8}
   [quo/separator]
   [react/view {:style {:background-color colors/gray-lighter
                        :width            40
                        :height           40
                        :border-radius    40
                        :border-width     4
                        :border-color     colors/white
                        :margin-top       -20
                        :margin-bottom    -20
                        :align-self       :center
                        :align-items      :center
                        :justify-content  :center}}
    [react/image {:source (icons/icon-source :main-icons/change)
                  :style  {:tint-color colors/gray
                           :transform  [{:rotate "90deg"}]}}]]])

(defn swap []
  (let [{:keys [name]}
        (<sub [:multiaccount/current-account])

        amount     "0.02"
        from-token (-> tokens/all-tokens-normalized
                       :mainnet
                       vals
                       first)
        to-token   (-> tokens/all-tokens-normalized
                       :mainnet
                       vals
                       second)]
    [kb-presentation/keyboard-avoiding-view {:style         {:flex            1
                                                             :justify-content :space-between}
                                             :ignore-offset true}
     [topbar/topbar
      {:title  name
       :modal? true}]

     [react/view {:flex               1
                  :padding-horizontal 16
                  :margin-vertical    32}
      [token-input {:amount   amount
                    :error    nil
                    :label    "Amount"
                    :token    from-token
                    :max-from 67.28}]

      [separator-with-icon]

      [token-input {:amount "0.01"
                    :error  nil
                    :label  "Minimum Received"
                    :token  to-token}]]


     [slider/animated-slider
      {:minimum-value 0
       :maximum-value 100}]
     [toolbar/toolbar
      {:show-border? true
       :left         [quo/button {:style {:background-color colors/white}}
                      "Swap and Send"]
       :right        [quo/button {:theme :accent}
                      "Swap"]}]]))

