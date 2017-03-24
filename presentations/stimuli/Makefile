build:
	hovercraft presentation.rst output

upload: build
	rsync -e "ssh -p 22" -P -rvzc --delete output/ nagasaki45@tomgurion.me:sites/public.tomgurion.me/saffran-stimuli-presentation --cvs-exclude
