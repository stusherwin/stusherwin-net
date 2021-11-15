require('./styles/main.scss');

let Navigation = {
  init: () => {
    let sections = document.querySelectorAll('.section');
    let currentMenuItem = document.getElementById('menu-current');
    let links = document.querySelectorAll('#menu a');

    if(!currentMenuItem || !links.length || !sections.length) {
      return;
    }

    let intersections = [];
    let sectionIds = [];

    let findIntersectingSection = entries => {
      entries.forEach(e => {
        var intersection = intersections.find(b => b.element == e.target);
        if(intersection) {
          if(e.isIntersecting) {
            intersection.ratio = e.intersectionRatio;
          } else {
            intersection.ratio = 0;
          }
        }
      });

      var which = null;
      intersections.forEach(intersection => {
        if(!which || intersection.ratio >= which.ratio) {
          which = intersection;
        }
      });

      if(which) {
        setCurrentSection(which.element);
      }
    };

    let observer = new IntersectionObserver(findIntersectingSection, {threshold:  [0, 0.25, 0.5, 0.75, 1]});

    sections.forEach(section => {
      intersections.push({element: section, ratio: 0});
      sectionIds.push(section.id);
      observer.observe(section);
    });

    let setCurrentSection = targetElement => {
      var section = targetElement;
      var contentIndex = sectionIds.indexOf(section.id);

      while (section && contentIndex < 0) {
        section = section.parentElement;
        contentIndex = sectionIds.indexOf(section.id);
      }

      if(!section || contentIndex < 0) {
        return;
      }
  
      let rect = links[contentIndex].getBoundingClientRect();
      let x = rect.x + (rect.width / 2);

      currentMenuItem.style.transform = 'translateX(' + x + 'px)';
    }
  
    links.forEach(link => {
      link.addEventListener("click", e => {
        let id = link.href.split('#')[1];
        let targetElement = document.getElementById(id);
        setCurrentSection(targetElement);
      });
    });
    
    window.addEventListener('resize', () => {
        findIntersectingSection(observer.takeRecords());
    });

    let initialSectionId = window.location.hash.length ? window.location.hash.substring(1) : sectionIds[0];
    let initialSection = document.getElementById(initialSectionId);

    if (initialSection) {
      setCurrentSection(initialSection);
    }
  }
};

let ImageGallery = {
  init: () => {
    let selectImage = (targetImage, gallery, images) => {
      var caption = gallery.querySelector('.caption > span');
      var currentIndex = parseInt(getComputedStyle(targetImage).getPropertyValue('--index'));
          
      [].forEach.call(images, image => {
        image.classList.remove('image-current');
        var index = parseInt(getComputedStyle(image).getPropertyValue('--index'));
        var newIndex = index;
        if (index === currentIndex) {
          newIndex = 0;
        } else if (index < currentIndex) {
          newIndex = index + 1;
        }
        image.style.setProperty('--index', '' + newIndex);
      });

      targetImage.classList.add('image-current');

      if(!caption) return;

      caption.innerHTML = targetImage.dataset.caption;
    };

    let setGalleryHeight = gallery => {
      let currentImage = gallery.getElementsByClassName('image-current')[0];
      let imagesContainer = gallery.getElementsByClassName('images')[0];

      if(!currentImage || !imagesContainer) {
        return;
      }

      let size = currentImage.getBoundingClientRect();
      imagesContainer.style.height = size.height + 'px';
    };

    let initialize = gallery => {
      let images = gallery.getElementsByClassName('image');
      let close = document.getElementById('fullscreen-close');

      gallery.style.setProperty('--no-images', '' + images.length);
  
      [].forEach.call(images, (image, i) => {
        image.style.setProperty('--index', '' + i);

        image.addEventListener('click', e => {
          e.preventDefault();

          if(image.classList.contains('image-current')) {
            if(gallery.classList.contains('fullscreen')) {
              gallery.classList.remove('fullscreen');
              document.body.append(close);
            } else {
              gallery.classList.add('fullscreen');
              image.append(close);
            }
          } else {
            selectImage(image, gallery, images);
          }
        });
      });

      window.addEventListener('resize', () => setGalleryHeight(gallery));
      selectImage(images[0], gallery, images);
      setGalleryHeight(gallery);
    };
  
    document.querySelectorAll('.image-gallery').forEach(initialize);  
  }
};

let ExpandContractLinks = {
  init: () => {
    document.querySelectorAll('.expand-contract').forEach(link => {
      link.addEventListener('click', e => {
        e.preventDefault();
  
        let paraClassess = link.previousElementSibling.classList;
        if(paraClassess.contains("fade-out")) {
          paraClassess.remove("fade-out");
        } else {
          paraClassess.add("fade-out");
        }
        let text = link.getElementsByTagName("span")[0];
        let altText = text.dataset.alt;
        text.dataset.alt = text.innerHTML;
        text.innerHTML = altText;
        
        let icon = link.getElementsByTagName("use")[0];
        let altIcon = icon.dataset.alt;
        icon.dataset.alt = icon.href.baseVal;
        icon.setAttribute('href', altIcon);
      });
    });
  }
};

let PcbTraces = {
  init: () => {
    let traceWidth = 8;

    let setTraceAngle = () => {
      var traces = document.querySelectorAll('.pcb-trace');
      let skewDegsByHeight = [];
      
      traces.forEach(t => {
        let parentSize = t.parentElement.getBoundingClientRect();
        
        if(!skewDegsByHeight[parentSize.height]) {
          let skewRads = Math.atan((parentSize.width - traceWidth) / parentSize.height);
          let skewDegs = skewRads * 180 / Math.PI;
    
          skewDegsByHeight[parentSize.height] = skewDegs;
        }
  
        t.style.setProperty('--skew', '' + skewDegsByHeight[parentSize.height])
      });
    };
  
    setTraceAngle();
  
    window.addEventListener('resize', setTraceAngle);
  }
}

document.addEventListener("DOMContentLoaded", () => {
  Navigation.init();
  ImageGallery.init();
  ExpandContractLinks.init();
  PcbTraces.init();
});
