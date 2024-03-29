(function(){
  let slideshow = remark.create({
    sourceUrl: 'presentation.md'
  });

  slideshow.on('showSlide', function(slide){
    if (MathJax && MathJax.typeset){
      MathJax.typeset();
    }
  })

  slideshow.on('showSlide', function(slide){
    const node = document.getElementById('primes');
    if (!!node) {
      const primesApp = Elm.Prime.init({node: node});
    }
  })
})();
