dir="${BASH_SOURCE[0]}";
if([ -h "${dir}" ]) then
  while([ -h "${dir}" ]) do dir=`readlink "${dir}"`; done
fi
pushd . > /dev/null
cd `dirname ${dir}` > /dev/null
dir=`pwd`;

LD_LIBRARY_PATH=$dir $dir/main
popd  > /dev/null
