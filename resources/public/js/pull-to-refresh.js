// Pull to refresh for the installed web app. A standalone PWA has no browser toolbar and, on iOS,
// no native pull-to-refresh, so the page could not be reloaded at all. In a normal browser tab the
// browser's own gesture stays in charge and this script does nothing.
(function () {
  var standalone = (window.matchMedia && window.matchMedia('(display-mode: standalone)').matches)
    || window.navigator.standalone === true;
  if (!standalone || !('ontouchstart' in window)) { return; }

  var THRESHOLD = 70;  // pull distance (after damping) that triggers the reload
  var MAX = 110;       // the indicator follows the finger at most this far
  var DAMPING = 0.5;   // the indicator moves half as far as the finger, like the native gesture

  var root = document.documentElement;
  root.classList.add('plauna-ptr');

  var indicator = document.createElement('div');
  indicator.className = 'plauna-ptr-indicator';
  indicator.setAttribute('aria-hidden', 'true');
  indicator.innerHTML =
    '<svg class="plauna-ptr-arrow" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5"'
    + ' stroke-linecap="round" stroke-linejoin="round"><path d="M12 5v14M5 12l7 7 7-7"/></svg>'
    + '<span class="plauna-ptr-spinner"></span>';

  var startX = 0, startY = 0, distance = 0;
  var tracking = false, pulling = false, refreshing = false, formDirty = false;

  // Unsaved input in a form would be lost by the reload.
  document.addEventListener('input', function (event) {
    if (event.target && event.target.form) { formDirty = true; }
  }, true);

  function scrollTop() {
    return (document.scrollingElement || root).scrollTop;
  }

  // A pull that starts in a scrolled inner list (e.g. a filter checklist), in a form field or while a
  // modal is open belongs to that element, not to the page.
  function startsInsideOtherScroller(target) {
    if (document.querySelector('dialog[open]')) { return true; }
    for (var el = target; el && el !== document.body && el.nodeType === 1; el = el.parentElement) {
      if (/^(INPUT|TEXTAREA|SELECT)$/.test(el.tagName) || el.isContentEditable) { return true; }
      if (el.scrollTop > 0) { return true; }
    }
    return false;
  }

  function render(dist, animate) {
    indicator.style.transition = animate ? 'transform 0.2s ease, opacity 0.2s ease' : 'none';
    indicator.style.transform = 'translate(-50%, ' + dist + 'px)';
    indicator.style.opacity = dist > 0 ? Math.min(1, dist / (THRESHOLD * 0.6)) : 0;
    var progress = Math.min(1, dist / THRESHOLD);
    indicator.classList.toggle('plauna-ptr-ready', dist >= THRESHOLD);
    indicator.firstChild.style.transform = 'rotate(' + (progress * 180) + 'deg)';
  }

  function reset() {
    tracking = false;
    pulling = false;
    distance = 0;
    render(0, true);
  }

  document.addEventListener('touchstart', function (event) {
    if (refreshing || event.touches.length !== 1 || scrollTop() > 0
        || startsInsideOtherScroller(event.target)) {
      tracking = false;
      return;
    }
    if (!indicator.parentNode) { document.body.appendChild(indicator); }
    startX = event.touches[0].clientX;
    startY = event.touches[0].clientY;
    tracking = true;
    pulling = false;
  }, { passive: true });

  document.addEventListener('touchmove', function (event) {
    if (!tracking) { return; }
    if (event.touches.length !== 1) { reset(); return; }
    var dx = event.touches[0].clientX - startX;
    var dy = event.touches[0].clientY - startY;
    if (!pulling) {
      // Leave horizontal swipes (wide tables) and upward scrolling alone.
      if (Math.abs(dx) > Math.abs(dy) || dy <= 0 || scrollTop() > 0) {
        if (Math.abs(dx) > 10 || dy < -10 || scrollTop() > 0) { tracking = false; }
        return;
      }
      if (dy < 8) { return; }
      pulling = true;
    }
    if (event.cancelable) { event.preventDefault(); }  // no iOS rubber band while pulling
    distance = Math.max(0, Math.min(MAX, dy * DAMPING));
    render(distance, false);
  }, { passive: false });

  function finish() {
    if (!pulling) { tracking = false; return; }
    if (distance < THRESHOLD
        || (formDirty && !window.confirm('Discard your unsaved changes and reload the page?'))) {
      reset();
      return;
    }
    tracking = false;
    pulling = false;
    refreshing = true;
    render(THRESHOLD, true);
    indicator.classList.add('plauna-ptr-refreshing');
    // A page shown as the answer to a form POST names the GET address to reload, so the
    // reload does not send the form again.
    var refreshUrl = document.querySelector('meta[name="plauna-refresh-url"]');
    if (refreshUrl) {
      window.location.replace(refreshUrl.getAttribute('content'));
    } else {
      window.location.reload();
    }
  }

  document.addEventListener('touchend', finish);
  document.addEventListener('touchcancel', function () { if (!refreshing) { reset(); } });

  // Coming back to the page from the back/forward cache must not leave the spinner running.
  window.addEventListener('pageshow', function (event) {
    if (event.persisted) {
      refreshing = false;
      indicator.classList.remove('plauna-ptr-refreshing');
      reset();
    }
  });
})();
