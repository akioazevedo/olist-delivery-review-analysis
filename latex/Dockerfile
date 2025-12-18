# 1. Base image with full TeX Live
FROM texlive/texlive:latest

# 2. Set a working directory inside the container
WORKDIR /work

# 3. Default command: compile main.tex twice (for TOC, refs, etc.)
CMD pdflatex -interaction=nonstopmode main.tex && \
    pdflatex -interaction=nonstopmode main.tex
