function download () {
    which wget
    if [ $? == 0 ]
    then
        wget $1
    else
        which curl
        if [ $? == 0 ]
        then
            curl $1 -o `basename $1`
        else
            echo "Can't find wget or curl. Don't know how to download files."
        fi
    fi
}

download 'http://www.tekepon.net/fsm/modules/refmap/images/map/TileA1.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/map/TileA2.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/map/TileA3.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/map/TileA4.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/map/TileA5.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/map/TileB.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/map/TileC.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/map/TileD.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/map/TileE.png'

download 'http://www.tekepon.net/fsm/modules/refmap/images/chara/vx_chara01_a.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/chara/vx_chara01_b.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/chara/vx_chara02_b.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/chara/vx_chara02_c.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/chara/vx_chara04_a.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/chara/vx_chara04_b.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/chara/vx_chara06_a.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/chara/vx_chara07_a.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/chara/vx_chara07_b.png'
download 'http://www.tekepon.net/fsm/modules/refmap/images/chara/vx_chara08_a.png'
