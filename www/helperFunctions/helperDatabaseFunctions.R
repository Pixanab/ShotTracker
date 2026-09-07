# helperDatabaseFunctions.R

db_get_collection_info_v2 <- function(connection_string, collectionIn){
  con = mongo(collection=collectionIn, db="PingItv1", url=connection_string)
  data <- con$find(field = '{}')
  return(data)
}

data_apply_filter <- function(data, column_name, filter_value) {
  filtered_data <- data %>%
    dplyr::filter(.data[[column_name]] == filter_value)
  return(filtered_data)
}

db_get_filtered_collection_v2 <- function(connection_string, collectionIn, column_name, filter){
  data <- db_get_collection_info_v2(connection_string, collectionIn)
  filtered_data <- data_apply_filter(data, column_name, filter)
  return(filtered_data)
}

db_insert <- function(connection_string, collectionIn, dataIn){
  # observe({print("Write to Collection")})
  # if(length(dataIn) > 0) {
  #   con = mongo(collection=collectionIn, db="PingItv1", url=connection_string)
  #   con$insert(dataIn)
  # }
  shinyalert("Adding shots is disabled in this version.", type = "warning")
  return
}

db_delete_many <- function(connection_string, collectionIn, ids) {
  # con <- mongo(collection = collectionIn, db = "PingItv1", url = connection_string)
  
  # if (length(ids) > 0) {
  #   # Format each ID for MongoDB
  #   formatted_ids <- sprintf('{"$oid":"%s"}', ids) # Where %s is a string placeholder for id's
  #   # Combine formatted IDs into a single string with commas
  #   id_filter <- sprintf('{"_id": {"$in": [%s]}}', 
  #                        paste(formatted_ids, collapse = ","))
  #   con$remove(id_filter)
  #   observe({print("Data successfully deleted.")})
  # }
  shinyalert("Deleting shots is disabled in this version.", type = "warning")
  return
}

db_update_id <- function(connection_string, collectionIn, idIn, dataIn){
  # observe({print("Updating Data")})
  # if(length(dataIn) > 0) {
  #   con = mongo(collection=collectionIn, db="PingItv1", url=connection_string)
  #   query = sprintf('{"_id":{"$oid":"%s"}}', idIn)
  #   data = '{'
  #   for(i in seq(length(dataIn))) {
  #     data = paste0(data, sprintf('"$set":{"%s":"%s"}', names(dataIn)[i], dataIn[i]))
  #     if(i < length(dataIn)) {
  #       data = paste0(data, ',')
  #     } else {
  #       data = paste0(data, '}')
  #     }
  #   }
  #   con$update(query, data)
  # }
  shinyalert("Editing shots is disabled in this version.", type = "warning")
  return
}

#Sets types in shots dataframe
cleanShotData <- function(dataIn)
{
  dataIn[, c(2,6,7,15:17)] <- sapply(dataIn[, c(2,6,7,15:17)], as.numeric)
  return(dataIn)
}
