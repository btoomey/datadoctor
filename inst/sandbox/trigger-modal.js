var screen1 = `<p>This column contains a set of unique integers do these represent:</p>
<div class="radio">
  <label>
    <input type="radio" name="optionsRadios" id="optionsRadios1" value="option1" checked>
    A set of unique identifier
  </label>
</div>
<div class="radio">
  <label>
    <input type="radio" name="optionsRadios" id="optionsRadios2" value="option2">
    A set of numeric values  one
  </label>
</div>
`

var screen2a = `<p>Since this is a unique ID, this column will not be useful in creating a predictive model that can predict new values, so will be marked as “do not use” for predictive models.</p>`

var screen2b = `<p>This column contains missing values, how do you want to address these missing values</p>
<div class="radio">
  <label>
    <input type="radio" name="screen2b" id="screen2b1" value="option1" checked>
    Impute a fixed value
  </label>
</div>
<div class="radio">
  <label>
    <input type="radio" name="screen2b" id="screen2b1" value="option2" checked>
     Impute with zero (when the values are strictly positive) or a value smaller than any of the existing values
  </label>
</div>
<div class="radio">
  <label>
    <input type="radio" name="screen2b" id="screen2b1" value="option3" checked>
    Impute the mean value of the non-missing values
  </label>
</div>
<div class="radio">
  <label>
    <input type="radio" name="screen2b" id="screen2b1" value="option4" checked>
    Impute the median value of the non-missing values
  </label>
</div>
<div class="radio">
  <label>
    <input type="radio" name="screen2b" id="screen2b1" value="option5" checked>
   Do nothing (this may complicate the creation of some model types)
  </label>
</div>
<div class="radio">
  <label>
    <input type="radio" name="screen2b" id="screen2b1" value="option6" checked>
  Do not use this column to create a model
  </label>
</div>
`

var buttons = {
  confirm: {
    label: 'Next',
    className: 'btn-success'
  },
  cancel: {
    label: 'Back',
    className: 'hidden'
  }
}


function triggerModal(){
  bootbox.confirm({ 
    size: "medium",
    title: "<span>Id</span><button class='btn btn-warning btn-sm pull-right'>Inspect</button>",
    message: screen1, 
    buttons: buttons,
    callback: function(result){ 
      /* result is a boolean; true = OK, false = Cancel*/ 
      if (result){
        var val1 = $('input[name="optionsRadios"]:checked').val();
        var screen2 = (val1 === 'option1') ? screen2a : screen2b
        bootbox.confirm({
          size: "medium",
          message: screen2,
          callback: function(result){
            
          }
        })
 
      }
    }
  })
}

$(function(){
  $('body').on('click', '.modal-click', triggerModal)
})
