
To fix defpure lint errors, try one of the following in `.clj-kondo/config.edn`

```
{:lint-as {defpure/defpure clj-kondo.lint-as/def-catch-all}}
{:lint-as {defpure/defpure clojure.core/defn}}
```
