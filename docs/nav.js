// Accessible dropdown: hover works via CSS, this adds click/keyboard/touch support
document.addEventListener('DOMContentLoaded', function () {
  document.querySelectorAll('.has-dropdown > .dropdown-toggle').forEach(function (btn) {
    btn.addEventListener('click', function (e) {
      var parent = btn.closest('.has-dropdown');
      var isOpen = parent.classList.contains('open');

      document.querySelectorAll('.has-dropdown.open').forEach(function (el) {
        if (el !== parent) {
          el.classList.remove('open');
          el.querySelector('.dropdown-toggle').setAttribute('aria-expanded', 'false');
        }
      });

      parent.classList.toggle('open', !isOpen);
      btn.setAttribute('aria-expanded', String(!isOpen));
    });
  });

  document.addEventListener('click', function (e) {
    if (!e.target.closest('.has-dropdown')) {
      document.querySelectorAll('.has-dropdown.open').forEach(function (el) {
        el.classList.remove('open');
        el.querySelector('.dropdown-toggle').setAttribute('aria-expanded', 'false');
      });
    }
  });

  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') {
      document.querySelectorAll('.has-dropdown.open').forEach(function (el) {
        el.classList.remove('open');
        el.querySelector('.dropdown-toggle').setAttribute('aria-expanded', 'false');
      });
    }
  });
});
