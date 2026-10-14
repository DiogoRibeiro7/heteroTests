import io, re
sp = r"C:\Users\diogo\AppData\Local\Temp\claude\c--Users-diogo-work-code-heteroTests\2af77d20-4404-4ce0-91a0-04d3bce97fda\scratchpad\t-wz"
repro = io.open(sp + r"\repro-tables.md", encoding="utf-8-sig").read().replace("\r\n", "\n").strip("\n")
study = io.open(sp + r"\study-tables.md", encoding="utf-8-sig").read().replace("\r\n", "\n").strip("\n")


def bold_headings(text):
    return re.sub(r"^### (.+)$", lambda m: "**" + m.group(1) + ".**", text, flags=re.M)


repro = bold_headings(repro)
study = bold_headings(study)

intro = io.open(sp + r"\readme-intro.md", encoding="utf-8").read().strip("\n")
middle = io.open(sp + r"\readme-middle.md", encoding="utf-8").read().strip("\n")
end = io.open(sp + r"\readme-end.md", encoding="utf-8").read().strip("\n")

section = intro + "\n\n" + repro + "\n\n" + middle + "\n\n" + study + "\n\n" + end + "\n"
io.open(sp + r"\readme-section.md", "w", encoding="utf-8", newline="\n").write(section)
print(len(section.splitlines()), "lines")
