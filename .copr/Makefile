srpm:
	mkdir /tmp/.copr
	tar cavf /tmp/.copr/`grep ^Source0: $(spec)|sed -e 's/^.* //'` --exclude-vcs -C .. .
	rpmbuild -bs $(spec) --define="_sourcedir /tmp/.copr" --define="_srcrpmdir $(outdir)"
