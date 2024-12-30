#!/bin/bash
lein javac

echo "Instructions:"
echo "  1. Open printed URLs in browser"
echo "  2. In Emacs: \`cider-connect-cljs\`"
echo "     Use printed port, \`shadow\` type, \`main\` build"
echo

npx shadow-cljs watch main
