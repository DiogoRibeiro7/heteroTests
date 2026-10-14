# Wang and Zhou (2007): stage 1, parked

Stage 1 of a consistent test of the variance function, following Wang and Zhou
(2007), was paused on 2026-10-12 and parked on this branch on 2026-10-14 so
that the work toward 1.0.0 comes first. It is to be resumed after 1.0.0. This
folder is not part of the package and must not be merged as it is.

## Source

Wang, L. and Zhou, X.-H. (2007), *Biometrics* 63(4), 1218–1225,
doi:10.1111/j.1541-0420.2007.00805.x. The article could not be obtained; the
prototype follows UW Biostatistics Working Paper 299 (7 September 2006, after
review), which the author accepted as the stage-1 source on 2026-10-12.
Compare it with the published article before any stage 2.

## What is done

- `inst/validation/variance-form-wz-prototype.R`: the statistic and the
  bootstrap in plain R. The working paper's equation (7) is misprinted
  (`-sum log g` for `-(1/2) sum log g`); the prototype uses the normal
  pseudo-likelihood of Carroll and Ruppert, as the design note explains.
- `inst/validation/variance-form-wz-reproduction.R` and `.csv`: the working
  paper's Table 1, reproduced with the Epanechnikov kernel (all 24 rates of
  T_n and all 6 Wald rates inside the 99% interval of the difference).
- `inst/validation/variance-form-wz-size-power.R`: the study script.
- `inst/validation/variance-form-wz-design.md`: the design note; section 6
  (findings and recommendation) is still a placeholder.
- Edits to `make-table.R`, the validation `README.md`, `ROADMAP.md` and the
  DNVK design note (the author's acceptance of the DNVK manuscript).
- `pass1/`: the 68 cells of pass 1 of the size and power study, 1000
  replications each (99% interval [0.032, 0.068]). Cells 1, 2, 3 and 6 were
  left to pass 2, which reruns them with the same seeds.

## What pass 1 suggests (not yet the gate)

At 150 observations two of the four Gaussian null cells rejected 0.069 (power
form with constant variance; exponential form with exponential variance), so
the gate at 5000 replications is likely to fail there. At 400 observations the
four cells were 0.042 to 0.060. Against a departure the added terms do not span
(a wave in the log variance) it had power 0.95 at 400 observations, against
0.06 to 0.12 for `performVarianceFormTest(var_formula = ~ x)` and 0.75 for the
DNVK prototype; it had less power than `performVarianceFormTest()` against the
forms that test was built for.

## To resume

1. Rebase this branch onto `main`; keep this folder out of any pull request.
2. Run pass 2, the 12 Gaussian null cells at 5000 replications, with
   `runner/worker.ps1` and `runner/launcher-pass2.ps1` and the queue in
   `runner/pass2-queue.txt` (cells 25 26 27 30 49 50 51 54 1 2 3 6, then cell
   4 as an equivalence check against `pass1/part-4.csv`, to be deleted before
   merging). About 2 hours on 6 workers with enough free memory; the workers
   pause below 4 GB free.
3. Merge pass 1 and pass 2 with `runner/merge.R` into
   `inst/validation/variance-form-wz-size-power.csv`, pass 2 overriding.
4. Render `make-table.R`, build the README section with
   `runner/readme-parts.py`, write section 6 of the design note and the
   ROADMAP entry, then commit by concern and open the stage 1 pull request.
