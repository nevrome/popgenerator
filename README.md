[![Build Status](https://travis-ci.com/nevrome/popgenerator.svg?token=vxsQ9RjxoGASGtX4Q8jc&branch=master)](https://travis-ci.com/nevrome/popgenerator)

## popgenerator

popgenerator is a population network generator.

```
popgenerator <path_to_config_file> <output_path_entities_list_file> <output_path_relations_list_file>
```

## Installation

```
git clone git@github.com:nevrome/popgenerator.git
wget https://github.com/vlang/v/archive/master.zip
unzip master.zip
cd v-master
make
cd ..
chmod +x merge.sh
./merge.sh
v-master/v popgenerator.v
```

## Citation

```
@Manual{clemens_schmid_popgenerator_2019,
  title = {Popgenerator: A Population Network Generator},
  author = {Clemens Schmid},
  year = {2019}
}
```
