# HaSS
Haskell SIMPLE Simulator

京都大学工学部情報学科計算機コース3回生配当『計算機科学実験及演習 3A』を補助するツールである。
基本的なSIMPLEアーキテクチャを命令レベルでシミュレートする`hass`コマンドと、資料にあるmnemonicに準拠したアセンブリを機械語へ変換する`hass-assembler`コマンドを提供する。

## Install
```
> git clone git@github.com:yu-i9/HaSS.git
*** 独自の拡張を加えている者は適宜プログラムを変更せよ***
> cd HaSS
> cabal configure --user
> cabal build
> cabal install
```

## Usage
```
hass <assembly.txt> <memory_init.mif>
 => 標準出力へ実行結果が出力される

hass-assembler <assembly.txt>
 => assembly.mif ファイルが同じディレクトリに作成される
```
 
## TODO
 - テストの追加
 - 逆アセンブラ
