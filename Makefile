PACKAGE			= autocv
SOURCE_DIR  = R/output
DEST_DIR		= ../cv
TARGET_HTML	= cv_matthewbain.html
TARGET_PDF	= cv_matthewbain.pdf

MSG         ?= Update using $(PACKAGE)
BRANCH      ?= dev

.PHONY: all roxy check docs cv cv-copy cv-commit cv-push cv-deploy cv-deploy-fast

all: roxy check

# -- CI ----------------------------------------------------------------------

# Consolidate package and documentation updates
roxy:
	Rscript -e "roxygen2::roxygenise()"

# Test build
check:
	Rscript -e "devtools::check(document = FALSE)"

# Preview docs
docs:
	Rscript -e "pkgdown::build_site()"

# -- cv ----------------------------------------------------------------------

cv:
	Rscript -e "$(PACKAGE)::render_cv_as_html(show = TRUE)"
	Rscript -e "$(PACKAGE)::render_cv_as_pdf()"

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
