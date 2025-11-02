FROM haskell:9.8

# 作業ディレクトリの設定
WORKDIR /app

# GHCとStackの情報を表示
RUN ghc --version && stack --version

# デフォルトコマンド
CMD ["/bin/bash"]
