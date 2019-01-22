"$CI_SCRIPTS_DIR"/retry.sh  cabal v2-update --index-state="@$(git show -s --format=%ct HEAD)"
