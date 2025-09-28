flower_plot

R script to generate a “flower” plot that summarizes set intersections at three levels:

Core – shared by all samples (center number)

Shared – present in ≥2 samples (inner half of each petal)

Singletons – unique to a single sample (outer half of each petal)

It’s a clear, compact alternative to Venn/UpSet plots, especially handy for pangenome summaries. Petals are grouped by category (e.g., species/family), colors scale to any number of groups, and the ring adapts its radius to the number of petals so layouts look good from 8 to 80+ samples.

Features

Single-polygon capsule petals (smooth tips, no seams) partly embedded in the ring

Grouped petals: all samples of the same group appear contiguously

Adaptive radius: ring size adjusts automatically to the number of samples

Readable palettes that scale (default: pastel_rainbow). Optional Glasbey via the pals pkg

Label auto-rotation, gentle typography, adjustable transparency (petal_alpha) that makes the inner half appear slightly darker over the ring

Simple CSV → plot helper

What’s in this repo

flower_plot_capsule.R – the plotting functions

sample CSVs:

flower_10_samples.csv

flower_20_samples.csv

exemplo_flower_plot.csv (PT-BR example)

Requirements

R ≥ 4.1

Packages: ggplot2, dplyr, purrr
(optional) pals (only if you want palette_mode="glasbey")

Install:

install.packages(c("ggplot2", "dplyr", "purrr"))
# optional for glasbey:
install.packages("pals")

Quick start
# 1) Load the script
source("flower_plot_capsule.R")

# 2) Draw from a CSV
p <- flower_plot_capsule_from_csv(
  "flower_20_samples.csv",
  core_value   = 42,                           # number shared by ALL samples
  group_order  = c("AbALV","Asfarvirus","Faustovirus","Kaumoebavirus","Pacmanvirus"),
  palette_mode = "soft_pastel",                # default; see Palettes section
  legend_title = "Family",
  label_offset = 0.9,
  petal_alpha  = 0.70
)

# 3) Show or save
print(p)
ggplot2::ggsave("flower_20.png", p, width = 7, height = 7, dpi = 300)


Tip: To write small texts under the center number, pass center_label = "example_text".

CSV format

The helper expects:

column	required	description
label	yes	sample name shown around the ring
shared	yes	count shared by ≥2 samples (plotted on inner half)
singletons	yes	count unique to this sample (plotted on outer half)
group	no	category for color & grouping (e.g., family/species)
order	no	integer to control order within each group

Minimal example (.csv):

label,shared,singletons,group,order
Sample 01,194,12,AbALV,1
Sample 02,361,125,Asfarvirus,1
Sample 03,308,71,Faustovirus,1
Sample 04,279,184,Kaumoebavirus,1
Sample 05,216,169,Pacmanvirus,1


If group is omitted, each sample gets its own color; if order is omitted, samples are ordered by input row within each group.

Function reference (essentials)
flower_plot_capsule(data, core_value, ...)

Data columns: label, shared, singletons; optional group, order

Grouping: group_order controls the order of contiguous groups around the ring

Colors:

palette_mode = "soft_pastel" (default), "pastel_rainbow", "dark_pastel", "okabe_ito", "glasbey", or "custom" (with palette=c(...))

legend_title to show legend; NULL hides it


Palettes

Default: pastel_rainbow – friendly and readable; scales to any number of groups

okabe_ito – color-blind friendly base set (extended with HCL hues if needed)

pastel_rainbow, dark_pastel – HCL-based smooth hues

glasbey – needs the pals package; great for many distinct groups

custom – pass a vector, e.g. palette=c("#4C78A8","#F58518","#54A24B")
(If you provide fewer colors than groups, the script automatically interpolates.)

Examples

From a data frame (no CSV):

source("flower_plot_capsule.R")

df <- data.frame(
  label = paste("S", 1:16),
  shared = sample(120:520, 16),
  singletons = sample(5:180, 16),
  group = rep(c("A","B","C","D"), each=4)
)

p <- flower_plot_capsule(
  data = df,
  core_value = 37,
  group_order = c("A","B","C","D"),
  palette_mode = "okabe_ito",
  legend_title = "Group",
  petal_alpha = 0.75,
  center_label = ""           # hide the text below the center number
)
print(p)


Rotate the entire flower by 10° and tighten spacing:

p <- flower_plot_capsule_from_csv(
  "flower_20_samples.csv",
  core_value = 42,
  angle_offset = 10 * pi/180,
  spacing_factor = 1.05
)

Troubleshooting

“Required column ‘…’ not found.”
Your CSV must have label, shared, singletons. Optionally group, order.

“ring_radius must be > petal_width/2.”
Increase ring_radius (or keep auto_radius=TRUE) or reduce petal_width.

Petals look too sparse or too crowded
Tweak spacing_factor (e.g., 1.05 for tighter, 1.25 for looser) or set auto_radius=FALSE and choose ring_radius manually.

Labels collide
Increase label_offset or set rotate_labels=FALSE and export larger.

Glasbey not found
Install pals (install.packages("pals")) or choose another palette.

Reproducible test CSVs
# 10 samples
set.seed(1)
df10 <- data.frame(
  label = paste("Sample", sprintf("%02d", 1:10)),
  shared = sample(150:360, 10),
  singletons = sample(8:200, 10),
  group = rep(c("AbALV","Asfarvirus","Faustovirus","Kaumoebavirus","Pacmanvirus"), length.out=10),
  order = ave(1:10, rep(1:5, length.out=10), FUN=seq_along)
)
write.csv(df10, "flower_10_samples.csv", row.names = FALSE)

# 20 samples
set.seed(2)
df20 <- data.frame(
  label = paste("Sample", sprintf("%02d", 1:20)),
  shared = sample(160:520, 20),
  singletons = sample(5:180, 20),
  group = rep(c("AbALV","Asfarvirus","Faustovirus","Kaumoebavirus","Pacmanvirus"), length.out=20),
  order = ave(1:20, rep(1:5, length.out=20), FUN=seq_along)
)
write.csv(df20, "flower_20_samples.csv", row.names = FALSE)

Citation

If this script helps your work, feel free to cite the repository URL and mention:
“flower_plot visualization for pangenome-like intersection summaries.”

License

MIT — do whatever you want, just keep the notice. See LICENSE
.

Contributing

Issues and PRs are welcome (bug fixes, features, palette ideas, docs improvements).
Please include a minimal CSV (or R snippet) that reproduces any bug you report.

Changelog

v0.1.0 – Initial public release: grouped petals, adaptive radius, scalable palettes, CSV helper.