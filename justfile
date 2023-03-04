rust FOLDER:
	cd $(zoxide query -i -- {{FOLDER}}); zellij -l ~/.config/zellij/rust_layout.kdl

stream-setup:
	zellij -l ~/.config/zellij/stream_layout.kdl

mp4-to-gif INPUT OUTPUT:
	ffmpeg -i INPUT -vf scale=460:-1 -r 10 -f image2pipe -vcodec ppm - | convert -delay 5 -loop 0 - OUTPUT