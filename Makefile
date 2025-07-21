PKG         := autocv
DEST_DIR    := ../cv
SOURCE_DIR  := R/output
TARGET_HTML := cv_matthewbain.html
TARGET_PDF  := cv_matthewbain.pdf

MSG         ?= Update using $(PKG)
BRANCH      ?= dev

.PHONY: all roxy check docs \
	check-typos \
	cv cv-copy cv-commit cv-push cv-deploy cv-deploy-fast \
	clean

all: roxy check

help:
	@echo "Please use \`make <target>' where <target> is one of:"
	@echo "  roxy             to sync package and documentation/resource updates"
	@echo "  check            to run R package QA checks"
	@echo "  docs             to build the package documentation site"
	@echo "  cv               to render your CV as HTML and PDF"
	@echo "  cv-deploy-fast   to sync the lastest CV version with the repo"
	@echo "                   [MSG='msg'] [BRANCH='branch']"
	@echo "  clean            to cleanup package manager caches"

# -- CI ----------------------------------------------------------------------

# Consolidate package and documentation/resource updates
# (R substitute for Python editable install: sync code, docstrings, vignettes)
roxy:
	Rscript -e "roxygen2::roxygenise()"
	R CMD INSTALL --preclean --no-multiarch --with-keep.source .

# Test build
check:
	Rscript -e "devtools::check(document = FALSE)"

# Preview docs
docs:
	Rscript -e "pkgdown::build_site()"

check-typos:
	@typos R/applications/ --no-ignore-vcs \
		--exclude 'input' --exclude '*_report_*' --exclude '*_counts_*' \
		--format brief

# -- cv ----------------------------------------------------------------------

cv:
	Rscript -e "$(PKG)::render_cv_as_html(show = TRUE)"
	Rscript -e "$(PKG)::render_cv_as_pdf()"

# Copy updated CV into its own repo and push
cv-copy:
	cp $(SOURCE_DIR)/$(TARGET_HTML) $(DEST_DIR)
	cp $(SOURCE_DIR)/$(TARGET_PDF) $(DEST_DIR)

# -- Dev ---------------------------------------------------------------------

# Compile all work at once (checkout-stage-commit)
# To use custom: make cv-commit MSG="your message" BRANCH="your-branch-name"
cv-commit:
	cd $(DEST_DIR) && \
	git checkout $(BRANCH) || git checkout -b $(BRANCH) && \
	git add . && \
	git commit -m "$$MSG"

# Push (compatible with rebasing workflows on an ongoing dev branch
# and merge conflict resolution).
# To use custom branch: make cv-push BRANCH="your-branch-name"
cv-push:
	cd $(DEST_DIR) && \
	git fetch origin && \
	git rebase origin/main || { echo "Rebase failed, resolve conflicts."; exit 1; } && \
	git push --set-upstream origin $(BRANCH)

# Full deployment: render CV, ask for confirmation, then copy, commit, push
# To use custom: make cv-deploy MSG="your message" BRANCH="your-branch-name"
# After running, clean up with:
# git checkout main; git pull; git branch -d <your-branch-name>
cv-deploy: cv
	@read -p "Deploy this CV version (HTML and PDF)? (y/n): " ans; \
	if [ "$$ans" = "y" ]; then \
		make cv-copy; \
		make cv-commit MSG="$(MSG)" BRANCH="$(BRANCH)"; \
		make cv-push BRANCH="$(BRANCH)"; \
	else \
		echo "Aborted."; \
	fi

# To use custom: make cv-deploy MSG="your message" BRANCH="your-branch-name"
cv-deploy-fast:
	open $(SOURCE_DIR)/$(TARGET_HTML) && open $(SOURCE_DIR)/$(TARGET_PDF)
	@read -p "Deploy this CV version (HTML and PDF)? (y/n): " ans; \
	if [ "$$ans" = "y" ]; then \
		make cv-copy; \
		make cv-commit MSG="$(MSG)" BRANCH="$(BRANCH)"; \
		make cv-push BRANCH="$(BRANCH)"; \
	else \
		echo "Aborted."; \
	fi

# XXX
clean:
	@echo "Cleaning up package manager caches..."
