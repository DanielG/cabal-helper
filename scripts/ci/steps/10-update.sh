update_args=
if ${CI_USE_COMMIT_INDEX_STATE:-true}; then
        update_args=--index-state="@$(git show -s --format=%ct HEAD)"
fi

"$CI_SCRIPTS_DIR"/retry.sh  cabal v2-update $update_args
