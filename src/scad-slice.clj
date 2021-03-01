(->> "slice/006.svg"
     f->path
     first
     (#(get-in % [1 :d]))
     (s/explain :svg-clj.main/path-string)
     println)
