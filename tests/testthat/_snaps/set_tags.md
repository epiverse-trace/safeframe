# tests for set_tags()

    Code
      set_tags(cars)
    Condition
      Error in `set_tags()`:
      ! Assertion on 'x' failed: Must inherit from class 'safeframe', but has class 'data.frame'.

---

    Code
      set_tags(x, toto = "speed")
    Condition
      Error in `base::tryCatch()`:
      ! 1 assertions failed:
       * Variable 'namedTag': Must be element of set {'speed','dist'}, but is
       * 'toto'.

