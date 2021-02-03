extractData = function(colName, varName, brfss_18, response = "Yes")
{
    t = brfss_18[brfss_18$Topic == colName, ]
    
    # Process sample size by adding up sample size for all types of responses
    Sample_Size = data.frame(matrix(ncol = 2, nrow = 55))
    colnames(Sample_Size) = c("Locationabbr", paste0("Sample_Size_", varName))
    rownames(Sample_Size) = unique(t$Locationabbr)
    for (i in unique(t$Locationabbr))
    {
        Sample_Size[i, "Locationabbr"] = i
        Sample_Size[i, paste0("Sample_Size_", varName)] = 
            sum(t[t$Locationabbr == i, "Sample_Size"])
    }
    
    t = t[t$Response == response, ]
    t = merge(t, Sample_Size, by = "Locationabbr")
    colNamesInUse = c("Year", "Locationabbr", "Locationdesc", "Data_value", 
                      "LocationID", paste0("Sample_Size_", varName))
    t = t[ , colNamesInUse]
    
    rownames(t) = t$LocationID
    t$Data_value = t$Data_value / 100
    colnames(t)[4] = varName
    new_t = t[order(row.names(t)), ]
    return (new_t)
}

extractDataQuestion = function(Question, varName, brfss_18, response = "Yes")
{
    t = brfss_18[brfss_18$Question == Question, ]
    
    Sample_Size = data.frame(matrix(ncol = 2, nrow = 55))
    colnames(Sample_Size) = c("Locationabbr", paste0("Sample_Size_", varName))
    rownames(Sample_Size) = unique(t$Locationabbr)
    for (i in unique(t$Locationabbr))
    {
        Sample_Size[i, "Locationabbr"] = i
        Sample_Size[i, paste0("Sample_Size_", varName)] = 
            sum(t[t$Locationabbr == i, "Sample_Size"])
    }
    
    t = t[t$Response == response, ]
    t = merge(t, Sample_Size, by = "Locationabbr")
    colNamesInUse = c("Year", "Locationabbr", "Locationdesc", "Data_value", 
                      "LocationID", paste0("Sample_Size_", varName))
    t = t[ , colNamesInUse]
    rownames(t) = t$LocationID
    t$Data_value = t$Data_value / 100
    colnames(t)[4] = varName
    return (t)
}

extractByAge = function(colName, varName, brfss_18, response = "Yes", AgeGroup)
{
    t = brfss_18[brfss_18$Topic == colName, ]
    t = t[t$Break_Out == AgeGroup, ]
    
    # Process sample size by adding up sample size for all types of responses
    Sample_Size = data.frame(matrix(ncol = 2, nrow = length(unique(t$Locationabbr))))
    colnames(Sample_Size) = c("Locationabbr", paste0("Sample_Size_", varName))
    
    rownames(Sample_Size) = unique(t$Locationabbr)
    for (i in unique(t$Locationabbr))
    {
        Sample_Size[i, "Locationabbr"] = i
        Sample_Size[i, paste0("Sample_Size_", varName)] = 
            sum(t[t$Locationabbr == i, "Sample_Size"])
    }
    
    t = t[t$Response == response, ]
    t = merge(t, Sample_Size, by = "Locationabbr")
    colNamesInUse = c("Locationabbr", "Data_value", 
                      "LocationID", paste0("Sample_Size_", varName), "StateID")
    t = t[ , colNamesInUse]
    
    rownames(t) = t$LocationID
    t$Data_value = t$Data_value / 100
    colnames(t)[2] = varName
    new_t = t[order(row.names(t)), ]
    return (new_t)
}

extract_G1 = function(TopicName, VarName, brfss_18, resp = "Yes")
{
    t1 = extractByAge(TopicName, VarName, brfss_18, AgeGroup = "18-24", response = resp)
    t2 = extractByAge(TopicName, VarName, brfss_18, AgeGroup = "25-34", response = resp)
    t3 = extractByAge(TopicName, VarName, brfss_18, AgeGroup = "35-44", response = resp)
    if (sum(is.na(t1)) != 0)
    {
        t1 = extractByAge(TopicName, VarName, brfss_18, AgeGroup = "18-24", response = "No")
        t1[[VarName]] = 1 - t1[[VarName]]
    }
    if (sum(is.na(t2)) != 0)
    {
        t2 = extractByAge(TopicName, VarName, brfss_18, AgeGroup = "25-34", response = "No")
        t2[[VarName]] = 1 - t2[[VarName]]
    }
    if (sum(is.na(t3)) != 0)
    {
        t3 = extractByAge(TopicName, VarName, brfss_18, AgeGroup = "35-44", response = "No")
        t3[[VarName]] = 1 - t3[[VarName]]
    }
    
    temp = cbind(t1[[VarName]], t2[[VarName]], t3[ , c(2, 5)])
    temp = merge(temp, Weights_Table, by = "StateID")
    rownames(temp) = temp$StateID
    
    ans = data.frame()
    for (i in rownames(temp))
        ans[i, VarName] = weighted.mean(temp[i, c(2:4)], temp[i, c(5:7)])
    
    return (ans)
}

extractByAge_Q = function(colName, varName, brfss_18, response = "Yes", AgeGroup)
{
    t = brfss_18[brfss_18$Question == colName, ]
    t = t[t$Break_Out == AgeGroup, ]

    # Process sample size by adding up sample size for all types of responses
    Sample_Size = data.frame(matrix(ncol = 2, nrow = length(unique(t$Locationabbr))))
    colnames(Sample_Size) = c("Locationabbr", paste0("Sample_Size_", varName))
    
    rownames(Sample_Size) = unique(t$Locationabbr)
    for (i in unique(t$Locationabbr))
    {
        Sample_Size[i, "Locationabbr"] = i
        Sample_Size[i, paste0("Sample_Size_", varName)] = 
            sum(t[t$Locationabbr == i, "Sample_Size"])
    }
    
    t = t[t$Response == response, ]
    t = merge(t, Sample_Size, by = "Locationabbr")
    colNamesInUse = c("Locationabbr", "Data_value", 
                      "LocationID", paste0("Sample_Size_", varName), "StateID")
    t = t[ , colNamesInUse]
    
    rownames(t) = t$LocationID
    t$Data_value = t$Data_value / 100
    colnames(t)[2] = varName
    new_t = t[order(row.names(t)), ]
    return (new_t)
}


extract_G1_Q = function(TopicName, VarName, brfss_18, resp = "Yes", resp_rev = "No")
{
    t1 = extractByAge_Q(TopicName, VarName, brfss_18, AgeGroup = "18-24", response = resp)
    t2 = extractByAge_Q(TopicName, VarName, brfss_18, AgeGroup = "25-34", response = resp)
    t3 = extractByAge_Q(TopicName, VarName, brfss_18, AgeGroup = "35-44", response = resp)

    if (sum(is.na(t1)) != 0)
    {
        t1 = extractByAge_Q(TopicName, VarName, brfss_18, AgeGroup = "18-24", response = resp_rev)
        t1[[VarName]] = 1 - t1[[VarName]]
    }
    if (sum(is.na(t2)) != 0)
    {
        t2 = extractByAge_Q(TopicName, VarName, brfss_18, AgeGroup = "25-34", response = resp_rev)
        t2[[VarName]] = 1 - t2[[VarName]]
    }
    if (sum(is.na(t3)) != 0)
    {
        t3 = extractByAge_Q(TopicName, VarName, brfss_18, AgeGroup = "35-44", response = resp_rev)
        t3[[VarName]] = 1 - t3[[VarName]]
    }
    
    temp = cbind(t1[[VarName]], t2[[VarName]], t3[ , c(2, 5)])
    temp = merge(temp, Weights_Table, by = "StateID")
    rownames(temp) = temp$StateID
    
    ans = data.frame()
    for (i in rownames(temp))
        ans[i, VarName] = weighted.mean(temp[i, c(2:4)], temp[i, c(5:7)])
    
    return (ans)
}

extract_G2_Q = function(TopicName, VarName, brfss_18, resp = "Yes", resp_rev = "No")
{
    t1 = extractByAge_Q(TopicName, VarName, brfss_18, AgeGroup = "45-54", response = resp)
    t2 = extractByAge_Q(TopicName, VarName, brfss_18, AgeGroup = "55-64", response = resp)
    
    if (sum(is.na(t1)) != 0)
    {
        t1 = extractByAge_Q(TopicName, VarName, brfss_18, AgeGroup = "45-54", response = resp_rev)
        t1[[VarName]] = 1 - t1[[VarName]]
    }
    if (sum(is.na(t2)) != 0)
    {
        t2 = extractByAge_Q(TopicName, VarName, brfss_18, AgeGroup = "55-64", response = resp_rev)
        t2[[VarName]] = 1 - t2[[VarName]]
    }

    temp = cbind(t1[[VarName]], t2[ , c(2, 5)])
    temp = merge(temp, Weights_Table, by = "StateID")
    rownames(temp) = temp$StateID
    ans = data.frame()
    for (i in rownames(temp))
        ans[i, VarName] = weighted.mean(temp[i, c(2:3)], temp[i, c(7:8)])
    
    return (ans)
}

extract_G2 = function(TopicName, VarName, brfss_18, resp = "Yes")
{
    t1 = extractByAge(TopicName, VarName, brfss_18, AgeGroup = "45-54", response = resp)
    t2 = extractByAge(TopicName, VarName, brfss_18, AgeGroup = "55-64", response = resp)
 
    if (sum(is.na(t1)) != 0)
    {
        t1 = extractByAge(TopicName, VarName, brfss_18, AgeGroup = "45-54", response = "No")
        t1[[VarName]] = 1 - t1[[VarName]]
    }
    if (sum(is.na(t2)) != 0)
    {
        t2 = extractByAge(TopicName, VarName, brfss_18, AgeGroup = "55-64", response = "No")
        t2[[VarName]] = 1 - t2[[VarName]]
    }

    temp = cbind(t1[[VarName]], t2[ , c(2, 5)])
    temp = merge(temp, Weights_Table, by = "StateID")
    rownames(temp) = temp$StateID
    print(temp)
    ans = data.frame()
    for (i in rownames(temp))
        ans[i, VarName] = weighted.mean(temp[i, c(2:3)], temp[i, c(7:8)])
    
    return (ans)
}

extract_G3_Q = function(TopicName, VarName, brfss_18, resp = "Yes", resp_rev = "No")
{
    t3 = extractByAge_Q(TopicName, VarName, brfss_18, AgeGroup = "65+", response = resp)
    
    if (sum(is.na(t3)) != 0)
    {
        t3 = extractByAge_Q(TopicName, VarName, brfss_18, AgeGroup = "65+", response = resp_rev)
        t3[[VarName]] = 1 - t3[[VarName]]
    }

    temp = t3[ , c(2, 5)]
    temp = merge(temp, Weights_Table, by = "StateID")
    rownames(temp) = temp$StateID
    
    ans = data.frame()
    for (i in rownames(temp))
        ans[i, VarName] = temp[i, 2]
    
    return (ans)
}

extract_G3 = function(TopicName, VarName, brfss_18, resp = "Yes")
{
    t3 = extractByAge(TopicName, VarName, brfss_18, AgeGroup = "65+", response = resp)
    if (sum(is.na(t3)) != 0)
    {
        t3 = extractByAge(TopicName, VarName, brfss_18, AgeGroup = "65+", response = "No")
        t3[[VarName]] = 1 - t3[[VarName]]
    }
    print(t3)
    temp = t3[ , c(2, 5)]
    temp = merge(temp, Weights_Table, by = "StateID")
    rownames(temp) = temp$StateID
    
    ans = data.frame()
    for (i in rownames(temp))
        ans[i, VarName] = temp[i, 2]
    
    return (ans)
}