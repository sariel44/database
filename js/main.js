function appendData(db, tables){

}
$(document).ready(function(){
$("#searchBtn").click(function( event ){
    searchWord = $("#searchField").val();
    $("#searchData").empty();
    $.getJSON("/search/" + searchWord, function(data){
        $.each(data, function(idx){
                (db,tables) = data[idx];
                $.each(tables)(function(table){
                    $("#searchData").append(table);
                })

        })
    })

    event.preventDefault();
})
})