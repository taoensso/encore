#!/bin/bash

clojure -Sdeps '{:deps {org.clojure/clojurescript {:mvn/version "1.12.42"} com.taoensso/encore {:mvn/version "3.149.0"}}}' -M -m cljs.main -re node
