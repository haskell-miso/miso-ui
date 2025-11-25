/* slider helpers */
function updateSlider (slider) {
  const min = parseFloat(slider.min || 0);
  const max = parseFloat(slider.max || 100);
  const value = parseFloat(slider.value);
  const percent = (max === min) ? 0 : ((value - min) / (max - min)) * 100;
  slider.style.setProperty('--slider-value', `${percent}%`);
};

globalThis.initSlider = function (slider) {
  updateSlider(slider);
  slider.addEventListener('input', function (event) {
   updateSlider(event.target)
  });
}

globalThis.deinitSlider = function (slider) {
  slider.removEventListener('input', function (event) {
    updateSlider(event.target)
  });
}
