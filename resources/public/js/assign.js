// Assigning suggested e-mails to a category on its page (category.html). Every e-mail goes through
// the same /metadata/category request as its own dropdown in the list - the move on the mail server
// included - one after the other. E-mails the mail server does not have are asked about once at the
// end. Assigned e-mails leave their group; an empty group leaves the page.
(function () {
  var texts = document.getElementById('category-texts');
  if (!texts) { return; }
  var data = texts.dataset;
  var category = data.category;
  var running = false;

  function text(key) {
    var value = data[key] || '';
    for (var i = 1; i < arguments.length; i++) { value = value.split('{' + (i - 1) + '}').join(arguments[i]); }
    return value;
  }

  function post(messageId, force) {
    var fields = {'message-id': messageId, 'category': category};
    if (force) { fields.force = 'true'; }
    return fetch('/metadata/category', {
      method: 'POST',
      headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      body: new URLSearchParams(fields)
    }).then(function (res) { return res.status; }, function () { return 0; });
  }

  function setBusy(busy) {
    running = busy;
    document.querySelectorAll('.assign-group, .assign-picked').forEach(function (button) { button.disabled = busy; });
  }

  function refreshGroup(group) {
    var left = group.querySelectorAll('.suggestion-email').length;
    if (left === 0) {
      group.remove();
      return;
    }
    var button = group.querySelector('.assign-group');
    button.textContent = button.dataset.label.split('{0}').join(left);
    var count = group.querySelector('.group-count');
    count.textContent = count.dataset.label.split('{0}').join(left);
  }

  async function assign(group, rows) {
    if (running || rows.length === 0) { return; }
    setBusy(true);
    var status = group.querySelector('.group-status');
    var counts = {assigned: 0, failed: 0, missing: 0};
    var missing = [];
    for (var i = 0; i < rows.length; i++) {
      status.textContent = text('assigning', i + 1, rows.length);
      var result = await post(rows[i].dataset.messageId, false);
      if (result === 204) { rows[i].remove(); counts.assigned++; }
      else if (result === 404) { missing.push(rows[i]); }
      else { counts.failed++; }
    }
    if (missing.length > 0) {
      status.textContent = '';
      var changeAnyway = window.confirm(text('missingConfirm', missing.length, data.categoryName));
      for (var j = 0; j < missing.length; j++) {
        if (changeAnyway && await post(missing[j].dataset.messageId, true) === 204) { missing[j].remove(); counts.assigned++; }
        else if (changeAnyway) { counts.failed++; }
        else { counts.missing++; }
      }
    }
    var parts = [text('done', counts.assigned)];
    if (counts.missing) { parts.push(text('missing', counts.missing)); }
    if (counts.failed) { parts.push(text('failed', counts.failed)); }
    status.textContent = parts.join(' ');
    setBusy(false);
    refreshGroup(group);
  }

  document.querySelectorAll('.suggestion-group').forEach(function (group) {
    group.querySelector('.assign-group').addEventListener('click', function () {
      assign(group, Array.prototype.slice.call(group.querySelectorAll('.suggestion-email')));
    });
    group.querySelector('.assign-picked').addEventListener('click', function () {
      assign(group, Array.prototype.filter.call(group.querySelectorAll('.suggestion-email'), function (row) {
        return row.querySelector('.suggestion-pick').checked;
      }));
    });
  });
})();
