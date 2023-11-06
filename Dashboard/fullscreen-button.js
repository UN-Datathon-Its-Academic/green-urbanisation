function openFullscreen(elem) {
  var map = $(elem).find('.leaflet.html-widget');
  if (document.fullscreenElement || document.mozFullScreenElement || document.msFullscreenElement || document.webkitFullscreenElement) {
    // If we are already in fullscreen, exit fullscreen mode
    if (document.exitFullscreen) {
      document.exitFullscreen();
    } else if (document.mozCancelFullScreen) { /* Firefox */
      document.mozCancelFullScreen();
    } else if (document.webkitExitFullscreen) { /* Chrome, Safari and Opera */
      document.webkitExitFullscreen();
    } else if (document.msExitFullscreen) { /* IE/Edge */
      document.msExitFullscreen();
    }
    $('#temperature_map').css('height', 'calc(100vh - 120px)');
  } else {
    // If we are not in fullscreen, request fullscreen
    if (elem.requestFullscreen) {
      elem.requestFullscreen();
    } else if (elem.mozRequestFullScreen) { /* Firefox */
      elem.mozRequestFullScreen();
    } else if (elem.webkitRequestFullscreen) { /* Chrome, Safari and Opera */
      elem.webkitRequestFullscreen();
    } else if (elem.msRequestFullscreen) { /* IE/Edge */
      elem.msRequestFullscreen();
    }
    $('#temperature_map').css('height', '100vh');
  }
}

document.addEventListener('fullscreenchange', exitHandler, false);
document.addEventListener('mozfullscreenchange', exitHandler, false);
document.addEventListener('MSFullscreenChange', exitHandler, false);
document.addEventListener('webkitfullscreenchange', exitHandler, false);

function exitHandler() {
  if (document.webkitIsFullScreen || document.mozFullScreen || document.msFullscreenElement) return;
  $('#map_container').css('height', '400px').trigger('resize');
  if (!document.fullscreenElement && !document.webkitIsFullScreen && !document.mozFullScreen && !document.msFullscreenElement) {
        $('#temperature_map').css('height', 'calc(100vh - 120px)');
    }
}
