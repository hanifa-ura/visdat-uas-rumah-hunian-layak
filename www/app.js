// Progres baca, header, menu mobile, penanda bab aktif, tombol ke atas, bar klaster yang menempel.
(function () {
  function resizeWidgets() { window.dispatchEvent(new Event('resize')); }

  document.addEventListener('DOMContentLoaded', function () {
    var root = document.documentElement;
    var header = document.querySelector('.site-header');
    var bar = document.getElementById('progress');
    var toTop = document.getElementById('to-top');

    function onScroll() {
      var max = root.scrollHeight - root.clientHeight;
      if (bar && max > 0) bar.style.width = (root.scrollTop / max * 100) + '%';
      if (header) header.classList.toggle('scrolled', root.scrollTop > 40);
      if (toTop) toTop.classList.toggle('show', root.scrollTop > 700);
    }
    window.addEventListener('scroll', onScroll, { passive: true });
    onScroll();
    if (toTop) toTop.addEventListener('click', function () {
      window.scrollTo({ top: 0, behavior: 'smooth' });
      var hero = document.getElementById('hero');
      if (hero) { hero.setAttribute('tabindex', '-1'); hero.focus({ preventScroll: true }); }
    });

    var toggle = document.querySelector('.nav-toggle');
    if (toggle && header) {
      var close = function () { header.classList.remove('open'); toggle.setAttribute('aria-expanded', 'false'); };
      toggle.addEventListener('click', function () {
        var open = header.classList.toggle('open');
        toggle.setAttribute('aria-expanded', open ? 'true' : 'false');
      });
      document.querySelectorAll('.site-nav a').forEach(function (a) { a.addEventListener('click', close); });
      document.addEventListener('keydown', function (e) {
        if (e.key === 'Escape' && header.classList.contains('open')) { close(); toggle.focus(); }
      });
    }

    if ('IntersectionObserver' in window) {
      var links = Array.prototype.slice.call(document.querySelectorAll('.site-nav a'));
      var spy = new IntersectionObserver(function (entries) {
        entries.forEach(function (e) {
          if (!e.isIntersecting) return;
          links.forEach(function (a) {
            var on = a.getAttribute('href') === '#' + e.target.id;
            a.classList.toggle('active', on);
            if (on) a.setAttribute('aria-current', 'true'); else a.removeAttribute('aria-current');
          });
        });
      }, { rootMargin: '-35% 0px -60% 0px' });
      links.forEach(function (a) {
        var s = document.querySelector(a.getAttribute('href'));
        if (s) spy.observe(s);
      });
    }

    // Bar klaster: tingginya disimpan di --clbar-h agar panel interpretasi tidak tertutup,
    // dan kelas "stuck" memberi bayangan hanya selama bar benar-benar mengapung.
    var clbar = document.querySelector('.cl-bar');
    if (clbar) {
      var setH = function () { root.style.setProperty('--clbar-h', clbar.offsetHeight + 'px'); };
      setH();
      window.addEventListener('resize', setH);
      if ('ResizeObserver' in window) new ResizeObserver(setH).observe(clbar);
      var checkStuck = function () {
        var hdr = parseFloat(getComputedStyle(root).getPropertyValue('--hdr-h')) || 0;
        var scope = clbar.parentElement.getBoundingClientRect();
        var top = clbar.getBoundingClientRect().top;
        clbar.classList.toggle('stuck', Math.abs(top - hdr) < 2 && scope.top < hdr);
      };
      window.addEventListener('scroll', checkStuck, { passive: true });
      checkStuck();
    }

    // Label kontrol segmen dihubungkan ke grup radionya untuk pembaca layar.
    document.querySelectorAll('.ctl').forEach(function (c) {
      var lbl = c.querySelector('.ctl-label[id]');
      var grp = c.querySelector('.shiny-input-radiogroup');
      if (lbl && grp) { grp.setAttribute('role', 'radiogroup'); grp.setAttribute('aria-labelledby', lbl.id); }
    });

    var t;
    window.addEventListener('orientationchange', function () { clearTimeout(t); t = setTimeout(resizeWidgets, 250); });
  });

  if (window.jQuery) {
    // Isi accordion baru dianggap terlihat oleh Shiny setelah details dibuka.
    document.addEventListener('toggle', function (e) {
      if (e.target && e.target.tagName === 'DETAILS' && e.target.open) jQuery(e.target).trigger('shown');
    }, true);
    // Peta di dalam conditionalPanel baru tahu ukurannya setelah dirender ulang.
    jQuery(document).on('shiny:value', function (e) {
      if (/^map_/.test(e.name)) setTimeout(resizeWidgets, 120);
    });
  }
})();
