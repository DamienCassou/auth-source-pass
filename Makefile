ELPA_DEPENDENCIES=package-lint let-alist

ELPA_ARCHIVES=melpa gnu

LINT_PACKAGE_LINT_FILES=auth-source-pass.el
TEST_ERT_FILES=test/auth-source-pass-tests.el
LINT_CHECKDOC_FILES=${LINT_PACKAGE_LINT_FILES} ${TEST_ERT_FILES}
LINT_COMPILE_FILES=${LINT_CHECKDOC_FILES}

makel.mk:
	# Download makel
	@if [ -f ../makel/makel.mk ]; then \
		ln -s ../makel/makel.mk .; \
	else \
		curl \
		--fail --silent --show-error --insecure --location \
		--retry 9 --retry-delay 9 \
		-O https://gitlab.petton.fr/DamienCassou/makel/raw/v0.5.3/makel.mk; \
	fi

# Include makel.mk if present
-include makel.mk
