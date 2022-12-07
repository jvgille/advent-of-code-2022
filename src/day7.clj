(ns day7 
  (:require
   [common :refer [pprint-spit]]
   [clojure.set]
   [clojure.string :refer [join split split-lines starts-with?]]))

(def example
 "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")


(defn is-command?
  [l]
  (starts-with? l "$"))

(defn is-dir?
  [l]
  (starts-with? l "dir"))

(defn is-ls?
  [l]
  (starts-with? l "$ ls")) 

(defn pop-dir
  [path]
  (case path
    "/" ""
    (or (first (split path #"/[^/]*$")) "/")))
  
(comment 
  (pop-dir "/123/abc/xyz")
  (pop-dir "/123/abc")
  (pop-dir "/123")
  (pop-dir "/") 
  )

(defn get-path-of
  [path filename]
  (case path
    "" filename
    "/" (join ["/" filename])
    (join "/" [path filename])))

(comment 
  (get-path-of "" "a")
  (get-path-of "" "/")
  (get-path-of "/" "abc")
  (get-path-of "/a" "abc")
  (get-path-of "/a/b" "abc")
  )

(defn update-path
  [path line]
  (if (is-ls? line)
    path
    (let [[_ _ dir] (split line #" ")]
      (if (= dir "..")
        (pop-dir path)
        (get-path-of path dir)))))

(comment 
  (update-path "" "$ cd /")
  (update-path "" "$ cd a")
  (update-path "/" "$ cd a")
  (update-path "/a" "$ cd b")
  )

(defn update-dirs
  [dirs path line]
  (let [dir-name (second (split line #" "))]
    (conj dirs (get-path-of path dir-name))))

(comment 
  (update-dirs #{"/a" "/b"} "/" "dir abc")
  (update-dirs #{"/a" "/b"} "/a" "dir a")
  (update-dirs #{"a" "b"} "" "dir b")
  )

(defn update-filemap
  [filemap path line] 
  (let [[size filename] (split line #" ")]
    (assoc filemap (get-path-of path filename) (Integer/parseInt size))))

(defn parse-line
  [dirs filemap path line]
  (if (is-command? line)
    [dirs filemap (update-path path line)]
    (if (is-dir? line)
      [(update-dirs dirs path line) filemap path] 
      [dirs (update-filemap filemap path line) path])))

(defn parse-filesizes
  [s] 
  (let [[dirs filemap]
        (reduce
         (fn [[dirs filemap path] line] (parse-line dirs filemap path line))
         [#{"/"} {} ""]
         (split-lines s))]
    [(apply list dirs) (apply list filemap)]))


(comment 
  (parse-filesizes example) 
  (parse-filesizes example)
  
  (pprint-spit "debugging.txt" (parse-filesizes (slurp "input/day7.txt"))) 
  )

(defn dir-contains?
  [dir file]
  (case dir
    "/" true
    (starts-with? file (join [dir "/"]))))

(comment
  (dir-contains? "/" "/a/e/i")
  (dir-contains? "/" "/foobar")
  (dir-contains? "/a" "/a/b.txt")
  (dir-contains? "/a" "/ab/c.txt")
  (dir-contains? "/a/b" "/a/a/b/c.txt")
  )

(defn count-directory-sizes
  [[dirs files]]
  (map (fn [dir]
         [dir (->> files
                   (filter (fn [[filename]] (dir-contains? dir filename)))
                   (map second)
                   (reduce +))] 
         ) dirs))

(comment 
  (count-directory-sizes (parse-filesizes example))
  (count-directory-sizes (parse-filesizes (slurp "input/day7.txt")))
  )

(defn part1 
  [s]
  (->>
   s
   (parse-filesizes)
   (count-directory-sizes)
   (map second)
   (filter #(<= % 100000))
   (reduce +)))

(comment
  (part1 example)
  (part1 (slurp "input/day7.txt")))

(def ^:private total-space 70000000)
(def ^:private free-space-required 30000000)

(defn part2
  [s]
  (let [directory-sizes (count-directory-sizes (parse-filesizes s))
        used-space (second (first (filter (fn [[dir]] (= dir "/")) directory-sizes)))
        available-space (- total-space used-space)
        missing-space (- free-space-required available-space)]
    (->>
     directory-sizes
     (map second)
     (filter #(< missing-space %))
     (apply min))))

(comment 
  (part2 example)
  (part2 (slurp "input/day7.txt")) 
  )
