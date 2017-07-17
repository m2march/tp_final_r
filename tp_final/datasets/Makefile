all: esac.db wjazzd.db wjazzd_mid

esac.db:
	wget http://jazzomat.hfm-weimar.de/download/esac.db

wjazzd.db:
	wget http://jazzomat.hfm-weimar.de/download/wjazzd.db


wjazzd_mid:
	wget http://jazzomat.hfm-weimar.de/download/RELEASE2.0_mid_unquant.zip
	mkdir $@
	unzip -d $@ RELEASE2.0_mid_unquant.zip 	
