window.onscroll = function() {scrollFunction()};

function scrollFunction() {
  if (document.documentElement.scrollTop > 300 || document.body.scrollTop > 300) {
    document.getElementById("scroll-top").style.display = "block";
  } else {
     document.getElementById("scroll-top").style.display = "none";
  }
};

// Handle the click event on the scroll-top button
function topFunction() {
  document.documentElement.scrollTop = 0;
  document.body.scrollTop = 0;
}
