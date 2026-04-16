document.addEventListener('DOMContentLoaded', function () {
  const searchInput = document.querySelector('[data-catalog-search]');
  const levelSelect = document.querySelector('[data-catalog-level]');
  const geoSelect = document.querySelector('[data-catalog-geo]');
  const cards = Array.from(document.querySelectorAll('[data-catalog-item]'));
  const applyFilter = function() {
    const q = (searchInput ? searchInput.value : '').toLowerCase().trim();
    const lvl = levelSelect ? levelSelect.value : '';
    const geo = geoSelect ? geoSelect.value : '';
    cards.forEach(function(card) {
      const txt = (card.getAttribute('data-search') || '').toLowerCase();
      const cardLvl = card.getAttribute('data-level') || '';
      const cardGeo = card.getAttribute('data-geo') || '';
      const okQ = !q || txt.indexOf(q) >= 0;
      const okL = !lvl || lvl === cardLvl;
      const okG = !geo || geo === cardGeo || geo === 'all';
      card.style.display = (okQ && okL && okG) ? '' : 'none';
    });
  };
  [searchInput, levelSelect, geoSelect].forEach(function(node) {
    if (node) node.addEventListener('input', applyFilter);
    if (node) node.addEventListener('change', applyFilter);
  });
  applyFilter();
  document.querySelectorAll('[data-figure-switch]').forEach(function(sel) {
    const group = sel.getAttribute('data-figure-switch');
    const sync = function() {
      document.querySelectorAll('[data-figure-panel="' + group + '"]').forEach(function(panel) {
        panel.classList.toggle('is-active', panel.getAttribute('data-figure-value') === sel.value);
      });
    };
    sel.addEventListener('change', sync);
    sync();
  });
  document.querySelectorAll('[data-tab-group]').forEach(function(group) {
    const name = group.getAttribute('data-tab-group');
    const buttons = Array.from(group.querySelectorAll('[data-tab-target]'));
    const panels = Array.from(group.querySelectorAll('[data-tab-panel]'));
    const activate = function(target) {
      buttons.forEach(function(btn) { btn.classList.toggle('is-active', btn.getAttribute('data-tab-target') === target); });
      panels.forEach(function(panel) { panel.classList.toggle('is-active', panel.getAttribute('data-tab-panel') === target); });
    };
    buttons.forEach(function(btn) { btn.addEventListener('click', function() { activate(btn.getAttribute('data-tab-target')); }); });
    if (buttons.length) activate(buttons[0].getAttribute('data-tab-target'));
  });
  const lb = document.createElement('div');
  lb.className = 'lightbox';
  lb.innerHTML = '<button type="button" aria-label="Cerrar">Cerrar</button><img alt="Figura ampliada">';
  document.body.appendChild(lb);
  const lbImg = lb.querySelector('img');
  const closeLb = function() { lb.classList.remove('is-open'); lbImg.removeAttribute('src'); };
  lb.querySelector('button').addEventListener('click', closeLb);
  lb.addEventListener('click', function(ev) { if (ev.target === lb) closeLb(); });
  document.querySelectorAll('.image-card img').forEach(function(img) {
    img.addEventListener('click', function() { lbImg.src = img.src; lb.classList.add('is-open'); });
  });
});
