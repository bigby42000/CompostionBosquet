function modfr() {
  $('body').on('shown.bs.modal', function() {
    $('.modal-dialog .modal-title').text('Crédits');
    $('.modal-dialog .modal-footer > button').text('Retour en arrière');
  });
}