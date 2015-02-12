# Experiencing a Leiningen Encore dependency conflict?

## Step 1 of 3: confirm a dependency conflict

Run `lein deps :tree`. That'll print a warning for any possible dependency conflicts.

## Step 2 of 3: resolve the conflict

Open your Leiningen `project.clj` file and look at your `:dependencies` section. A dependency conflict is normally caused by a situation like this:

```clojure
:dependencies [[library-A-with-OLD-encore-dep "version"] ; A above B (sad panda)
               [library-B-with-NEW-encore-dep "version"]
               ]
```

If library A and B both pull in an Encore dependency and they're requesting different versions, Leiningen will choose the version requested by the **library listed first**. What we _actually_ want is just the **newest** version, always [1].

> [1] Unfortunately there isn't currently any reliable way of instructing Leiningen to just prefer the newest version in the event of a dep conflict, which is why we need to resolve this manually.

You can resolve the conflict in two ways:

#### Step 2 of 3, method 1

Just reorder your dependencies:

```clojure
:dependencies [[library-B-with-NEW-encore-dep "version"] ; B above A (fixed!)
               [library-A-with-OLD-encore-dep "version"]
               ]
```

#### Step 2 of 3, method 2

Or you can place an explicit Encore dependency near the top of your `:dependencies`, above any other `com.taoensso` libraries:

```clojure
:dependencies [[com.taoensso/encore           "latest-version"]
               [library-A-with-OLD-encore-dep "version"]
               [library-B-with-NEW-encore-dep "version"]
               ]
```

For this method you'll be responsible for always making sure that you're using the [latest version](https://clojars.org/com.taoensso/encore) of Encore though. So if you do go this route, you may find [lein-ancient](https://github.com/xsc/lein-ancient) handy.

## Step 3 of 3: CLEAN YOUR BROKEN BUILD ARTIFACTS

Don't forget to run `lein clean` to clean up any broken Clojure/Script build artifacts.

Aaand you should be good to go. Run `lein deps :tree` again to confirm. Happy hacking! :-)
