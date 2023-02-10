(ns ch04-state-id.core
  (:gen-class))

;;; state, identity, and change

;; tools for managing change

;; atom

(defn go-shopping-naive
  "returns a list of items purchased"
  [shopping-list]
  (loop [[item & items] shopping-list
         cart []]
    (if item
      (recur items (conj cart item))
      cart)))

(def inventory (atom {}))

(defn no-negative-values?
  "check values of a map for a negative value"
  [m]
  (not-any? neg? (vals m)))

(defn in-stock?
  "check if an item is in stock"
  [item]
  (let [cnt (item @inventory)]
    (and (pos? cnt))))

(defn init
  "set up store with inventory"
  [items]
  (set-validator! inventory no-negative-values?)
  (swap! inventory items))

(defn grab
  "grab an item from the shelf"
  [item]
  (if (in-stock? item)
    (swap! inventory update-in [item] dec)))

(defn stock
  "stock an item on the shelf"
  [item]
  (swap! inventory update-in [item] inc))

(defn shop-for-item
  "shop for an item, return updated cart"
  [cart item]
  (if (grab item)
    (conj cart item)
    cart))

(defn go-shopping
  "return a list of items purchased"
  [shopping-list]
  (reduce shop-for-item [] shopping-list))

;; watching inventory
(declare sold-items)

(defn restock-order
  "a watch to restock an item. k: key, r: reference, ov: old value, nv: new value."
  [k r ov nv]
  (doseq [item (for [kw (keys ov)
                     :when (not= (kw ov) (kw nv))] kw)]
    (swap! sold-items update-in [item] (fnil inc 0))
    (println "need to restock" item)))

(defn init-with-restock
  "set up store with inventory"
  [m]
  (def inventory (atom m))
  (def sold-items (atom {}))
  (set-validator! inventory no-negative-values?)
  (add-watch inventory :restock restock-order))

(defn restock-all
  "restock all items sold"
  []
  (swap! inventory #(merge-with + % @sold-items))
  (reset! sold-items {}))

(init-with-restock {:apples 1 :bacon 3 :milk 2})
;; => #atom[{:apples 1, :bacon 3, :milk 2} 0x5b1257de]

(grab :bacon)
;; need to restock :bacon
;; => {:apples 1, :bacon 2, :milk 2}

(grab :bacon)
;; need to restock :bacon
;; => {:apples 1, :bacon 1, :milk 2}

(grab :milk)
;; need to restock :milk
;; => {:apples 1, :bacon 1, :milk 1}

@sold-items
;; => {:bacon 2, :milk 1}

@inventory
;; => {:apples 1, :bacon 1, :milk 1}

(restock-all)
;; need to restock :bacon
;; need to restock :milk
;; => {}

@inventory
;; => {:apples 1, :bacon 3, :milk 2}

;;; transactional change with ref
(def shopping-list (ref #{}))
(def assignments (ref {}))
(def shopping-cart (ref #{}) :validator #(not (contains? % :candy)))

(def my-kids #{:alice :bobby :cindy})

(defn born! [new-kid]
  (alter-var-root #'my-kids conj new-kid))

(defn notify-parent
  [k r _ nv]
  (if (contains? nv :candy)
    (println "there is candy in the cart")))

(defn init-list []
  (init {:eggs 2 :bacon 3 :apples 3
         :candy 5 :soda 2 :milk 1
         :bread 3 :carrots 1 :potatoes 1
         :cheese 3})
  (dosync
   (ref-set shopping-list #{:milk :butter :bacon :eggs
                            :carrots :potatoes :cheese :apples})
   (ref-set assignments {})
   (ref-set shopping-carts #{}))
  (add-watch shopping-cart :candy notify-parent))

(defn assignment [child]
  (get @assignments child))

(defn buy-candy []
  (dosync
   (commute shopping-cart conj (grab :candy))))

(comment
 (defn dawdle
   "screw around, get lost, maybe buy candy"
   []
   (let [t (rand-int 5000)]
     (Thread/sleep t)
     (maybe? buy-candy))))

(defn collect-assignment [child]
  (let [item (assignment child)]
    (dosync
     (alter shopping-cart conj item)
     (alter assignments dissoc child)
     (ensure shopping-list))
    item))

(defn assign-item-to-child [child]
  (let [item (first @shopping-list)]
    (dosync
     (alter assignments assoc child item)
     (alter shopping-list disj item))
    item))

;; making the trip
(defn send-child-for-item
  "eventually shop for an item"
  [child item q]
  (println child "is searching for" item)
  ;;(dawdle)
  (collect-assignment child)
  (>!! q child))

(defn report []
  (println "store inventory" @inventory)
  (println "shopping-list" @shopping-list)
  (println "assignments" @assignments)
  (println "shopping-cart" @shopping-cart))

(defn go-shopping-2 []
  (init)
  (report)
  (let [kids (chan 10)]
    (doseq [k my-kids]
      (>!! kids k))
    (go-loop [kid (<! kids)]
      (if (seq @shopping-list)
        (do
          (go
            (send-child-for-item kid (assign-item-to-child kid) kids))
          (recur (<! kids)))
        (do
          (println "done shopping.")
          (report))))))

