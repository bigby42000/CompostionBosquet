function bookmark_fr() {
  $('body').on('shown.bs.modal', function() {
    $('.modal-dialog .modal-title').text('Lien');
    $('.modal-dialog .modal-body > span:first').empty();
    $('#shiny-bookmark-copy-text').text('Appuyer sur Ctrl+C pour copier le lien.');
    $('.modal-dialog .modal-footer > button').text('Retour en arrière');
  });
}