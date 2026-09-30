## How to use

1.  Upload a Kaplan-Meier image.
2.  Prompt the model.
Say something like:

> Reconstruct the data from the uploaded image.

In the prompt, you can provide extra details about the image you want the agent to know, such as the total number of events for each Kaplan-Meier curve in the image.
You can also give it feedback to nudge it in the right direction, or ask questions about the image and the reconstruction process.
Visit the [`retroglyph` R package documentation](https://wlandau.github.io/retroglyph) for the technical details.

## Examples to try

Here are a few example Kaplan-Meier images to try downloading and reconstructing:

- [Simulated example](images/simulation.png)
- [Rosenstock et al. (2019)](https://doi.org/10.1001/jama.2018.18269) Figure 2
- [Tap et al. (2020)](https://doi.org/10.1001/jama.2020.1707) Figure 2A
- [Wiviott et al. (2019)](https://doi.org/10.1056/NEJMoa1812389) Figure 1
- [Pfeffer et al. (2015)](https://doi.org/10.1056/NEJMoa1509225) Figure 1

## Limitations

The agent relies on the following assumptions about the input image:

1.  Each KM curve is a solid line (no dots or dashes) with a single unique color, and all curve colors are visually distinct (no color mixing due to transparency with overlap).
2.  There is one and only one unique x axis.
Likewise, there is one and only one unique y axis.
3.  The x and y axes are solid, contiguous, perfectly horizontal/vertical lines that cross at the bottom-left corner of the plotting panel, with increasing linear scales.
4.  The y axis is survival or cumulative incidence, on a probability or percentage scale.
5.  The image must contain a risk table.
6.  The image text must be clear and detailed enough for OCR to read the axis tick labels and risk table.
This requires high enough resolution and large enough font.

## Troubleshooting

If data reconstruction fails, a retry in a fresh app session often fixes the issue because the underlying visual language model is stochastic.
If the error is persistent and you believe `retroglyph` should be able to support the image, please [file an issue](https://github.com/wlandau/retroglyph/issues) and share details that help with troubleshooting (the image, the error message, and the time and date you accessed the app).
