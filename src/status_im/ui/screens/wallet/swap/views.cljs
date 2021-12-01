(ns status-im.ui.screens.wallet.swap.views
  (:require [quo.core :as quo]
            [quo.design-system.colors :as colors]
            [re-frame.core :as re-frame]
            [status-im.ethereum.tokens :as tokens]
            [status-im.i18n.i18n :as i18n]
            [status-im.ui.components.chat-icon.screen :as chat-icon]
            [status-im.ui.components.icons.icons :as icons]
            [status-im.ui.components.keyboard-avoid-presentation
             :as
             kb-presentation]
            [status-im.ui.components.react :as react]
            [status-im.ui.components.search-input.view :as search-input]
            [status-im.ui.components.slider :as slider]
            [status-im.wallet.swap.core :as wallet.swap]
            [status-im.ui.components.toolbar :as toolbar]
            [status-im.ui.components.topbar :as topbar]
            [status-im.ui.screens.wallet.components.views :as wallet.components]
            [status-im.utils.handlers :refer [<sub]]
            [status-im.wallet.utils :as wallet.utils]))

(defn render-asset [{{:keys
                      [icon decimals amount color value]
                      :as token} :token
                     currency    :currency
                     on-press    :on-press}]
  [quo/list-item
   {:title               [quo/text {:weight :medium}
                          [quo/text {:weight :inherit}
                           (str (if amount
                                  (wallet.utils/format-amount amount decimals)
                                  "...")
                                " ")]
                          [quo/text {:color  :secondary
                                     :weight :inherit}
                           (wallet.utils/display-symbol token)]]
    :on-press            on-press
    :subtitle            (str (if value value "...") " " currency)
    :accessibility-label (str (:symbol token) "-asset-value")
    :icon                (if icon
                           [wallet.components/token-icon icon]
                           [chat-icon/custom-icon-view-list (:name token) color])}])

(defn asset-selector []
  (let [{:keys [address]} (<sub [:multiaccount/current-account])
        {:keys [tokens]}  (<sub [:wallet/visible-assets-with-values address])
        source?           (<sub [:wallet/modal-selecting-source-token?])
        currency          (<sub [:wallet/currency])]
    [:<>
     [topbar/topbar
      {:title  (if source?
                 (i18n/label :t/select-token-to-swap)
                 (i18n/label :t/select-token-to-receive))
       :modal? true}]

     [search-input/search-input
      {:search-active? true}]

     [react/scroll-view
      (for [token tokens]
        ^{:key (:name token)}
        [render-asset {:token    token
                       :on-press #(re-frame/dispatch
                                   [(if source?
                                      ::wallet.swap/set-from-token
                                      ::wallet.swap/set-to-token)
                                    (:symbol token)])
                       :currency (:code currency)}])]]))

(defn token-display
  "Show token and act as an anchor to open selector."
  [{:keys [token source?]}]
  (let [token-icon-source (-> token :icon :source)]
    [react/touchable-highlight
     {:on-press #(re-frame/dispatch [::wallet.swap/open-asset-selector-modal source?])}
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
  [{:keys [amount error label token max-from source?]}]
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
     [token-display {:token   token
                     :source? source?}]]))

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

        all-tokens  (<sub [:wallet/all-tokens]) 
        from-symbol (<sub [:wallet/swap-from-token])
        to-symbol   (<sub [:wallet/swap-to-token])
        amount      "0.02"
        from-token  (tokens/symbol->token all-tokens (or from-symbol :DGX))
        to-token    (tokens/symbol->token all-tokens (or to-symbol :SNT))]

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
                    :source?  true
                    :max-from 67.28}]

      [separator-with-icon]

      [token-input {:amount  "0.01"
                    :error   nil
                    :label   "Minimum Received"
                    :source? false
                    :token   to-token}]]


     [slider/animated-slider
      {:minimum-value 0
       :maximum-value 100}]
     [toolbar/toolbar
      {:show-border? true
       :left         [quo/button {:style {:background-color colors/white}}
                      "Swap and Send"]
       :right        [quo/button {:theme :accent}
                      "Swap"]}]]))

