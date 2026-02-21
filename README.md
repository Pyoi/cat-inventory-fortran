# cat-inventory-fortran
猫の餌
# Cat Inventory Manager (Fortran)

自宅の猫用フードと猫砂の在庫を管理しますが、1匹用ですので適宜修正予定

## Features

１在庫の購入/消費管理
２在庫1以下で赤表示
３ファイル保存（日本語キー対応）
　　※動かないパソコンもありそうですので別のコードでリメイク予定

## Build

gfortran を使用：

```bash
gfortran cat_inventory_manager.f90 -o cat_inventory
